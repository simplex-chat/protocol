{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store
  ( SQLiteStore,
    StoreError (..),
    createStore,
    createUser,
    getUsers,
    setActiveUser,
    createDirectConnection,
    createDirectContact,
    deleteContact,
    getContact,
    getContactConnections,
    getConnectionChatDirection,
    updateConnectionStatus,
    createNewGroup,
    createGroupInvitation,
    getGroup,
    getGroupInvitation,
    createContactGroupMember,
    createMemberConnection,
    updateGroupMemberStatus,
    createNewGroupMember,
    createIntroductions,
    updateIntroStatus,
    saveIntroInvitation,
    createIntroReMember,
    createIntroToMemberContact,
    saveMemberInvitation,
  )
where

import Control.Concurrent.STM (stateTVar)
import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Crypto.Random (ChaChaDRG, randomBytesGenerate)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (find, sortBy)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), SQLError, (:.) (..))
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (AParty (..), ConnId, SMPQueueInfo)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), createSQLiteStore, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import Simplex.Messaging.Util (bshow, liftIOEither)
import System.FilePath (takeBaseName, takeExtension)
import UnliftIO.STM

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations =
  sortBy (compare `on` name) . map migration . filter sqlFile $
    $(makeRelativeToProject "migrations" >>= embedDir)
  where
    sqlFile (file, _) = takeExtension file == ".sql"
    migration (file, qStr) = Migration {name = takeBaseName file, up = decodeUtf8 qStr}

createStore :: FilePath -> Int -> IO SQLiteStore
createStore dbFilePath poolSize = createSQLiteStore dbFilePath poolSize migrations

checkConstraint :: StoreError -> IO (Either StoreError a) -> IO (Either StoreError a)
checkConstraint err action = action `E.catch` (pure . Left . handleSQLError err)

handleSQLError :: StoreError -> SQLError -> StoreError
handleSQLError err e
  | DB.sqlError e == DB.ErrorConstraint = err
  | otherwise = SEInternal $ bshow e

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid()"

type StoreMonad m = (MonadUnliftIO m, MonadError StoreError m)

createUser :: StoreMonad m => SQLiteStore -> Profile -> Bool -> m User
createUser st Profile {displayName, fullName} activeUser =
  liftIOEither . checkConstraint SEDuplicateName . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO users (local_display_name, active_user, contact_id) VALUES (?, ?, 0)" (displayName, activeUser)
    userId <- insertedRowId db
    DB.execute db "INSERT INTO display_names (local_display_name, ldn_base, user_id) VALUES (?, ?, ?)" (displayName, displayName, userId)
    DB.execute db "INSERT INTO contact_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
    profileId <- insertedRowId db
    DB.execute db "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, is_user) VALUES (?, ?, ?, ?)" (profileId, displayName, userId, True)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?" (contactId, userId)
    pure . Right $ toUser (userId, contactId, activeUser, displayName, fullName)

getUsers :: SQLiteStore -> IO [User]
getUsers st =
  withTransaction st $ \db ->
    map toUser
      <$> DB.query_
        db
        [sql|
          SELECT u.user_id, u.contact_id, u.active_user, u.local_display_name, p.full_name
          FROM users u
          JOIN contacts c ON u.contact_id = c.contact_id
          JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
        |]

toUser :: (UserId, Int64, Bool, ContactName, Text) -> User
toUser (userId, userContactId, activeUser, displayName, fullName) =
  let profile = Profile {displayName, fullName}
   in User {userId, userContactId, localDisplayName = displayName, profile, activeUser}

setActiveUser :: MonadUnliftIO m => SQLiteStore -> UserId -> m ()
setActiveUser st userId = do
  liftIO . withTransaction st $ \db -> do
    DB.execute_ db "UPDATE users SET active_user = 0"
    DB.execute db "UPDATE users SET active_user = 1 WHERE user_id = ?" (Only userId)

createDirectConnection :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> m ()
createDirectConnection st userId agentConnId =
  liftIO . withTransaction st $ \db ->
    void $ createConnection_ db userId agentConnId Nothing 0

createConnection_ :: DB.Connection -> UserId -> ConnId -> Maybe Int64 -> Int -> IO Connection
createConnection_ db userId agentConnId viaContact connLevel = do
  createdAt <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO connections
        (user_id, agent_conn_id, conn_status, conn_type, via_contact, conn_level, created_at) VALUES (?,?,?,?,?,?,?);
    |]
    (userId, agentConnId, ConnNew, ConnContact, viaContact, connLevel, createdAt)
  connId <- insertedRowId db
  pure Connection {connId, agentConnId, connType = ConnContact, entityId = Nothing, viaContact, connLevel, connStatus = ConnNew, createdAt}

createDirectContact :: StoreMonad m => SQLiteStore -> UserId -> Connection -> Profile -> m ()
createDirectContact st userId Connection {connId} profile =
  void $
    liftIOEither . withTransaction st $ \db ->
      createContact_ db userId connId profile

createContact_ :: DB.Connection -> UserId -> Int64 -> Profile -> IO (Either StoreError (Text, Int64, Int64))
createContact_ db userId connId Profile {displayName, fullName} =
  withLocalDisplayName db userId displayName $ \ldn -> do
    DB.execute db "INSERT INTO contact_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
    profileId <- insertedRowId db
    DB.execute db "INSERT INTO contacts (contact_profile_id, local_display_name, user_id) VALUES (?, ?, ?)" (profileId, ldn, userId)
    contactId <- insertedRowId db
    DB.execute db "UPDATE connections SET contact_id = ? WHERE connection_id = ?" (contactId, connId)
    pure (ldn, contactId, profileId)

deleteContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ContactName -> m ()
deleteContact st userId displayName =
  liftIO . withTransaction st $ \db -> do
    DB.executeNamed
      db
      [sql|
        DELETE FROM connections WHERE connection_id IN (
          SELECT connection_id
          FROM connections c
          JOIN contacts cs ON c.contact_id = cs.contact_id
          WHERE cs.user_id = :user_id AND cs.local_display_name = :display_name
        )
      |]
      [":user_id" := userId, ":display_name" := displayName]
    DB.executeNamed
      db
      [sql|
        DELETE FROM contacts
        WHERE user_id = :user_id AND local_display_name = :display_name
      |]
      [":user_id" := userId, ":display_name" := displayName]
    DB.executeNamed
      db
      [sql|
        DELETE FROM display_names
        WHERE user_id = :user_id AND local_display_name = :display_name
      |]
      [":user_id" := userId, ":display_name" := displayName]

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getContact ::
  StoreMonad m => SQLiteStore -> UserId -> ContactName -> m Contact
getContact st userId localDisplayName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    c@Contact {contactId} <- getContact_ db
    activeConn <- getConnection_ db contactId
    pure $ (c :: Contact) {activeConn}
  where
    getContact_ db = ExceptT $ do
      toContact
        <$> DB.queryNamed
          db
          [sql|
            SELECT c.contact_id, p.display_name, p.full_name
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = :user_id AND c.local_display_name = :local_display_name AND c.is_user = :is_user
          |]
          [":user_id" := userId, ":local_display_name" := localDisplayName, ":is_user" := False]
    getConnection_ db contactId = ExceptT $ do
      connection
        <$> DB.queryNamed
          db
          [sql|
            SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
              c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
            FROM connections c
            WHERE c.user_id = :user_id AND c.contact_id == :contact_id
            ORDER BY c.connection_id DESC
            LIMIT 1
          |]
          [":user_id" := userId, ":contact_id" := contactId]
    toContact [(contactId, displayName, fullName)] =
      let profile = Profile {displayName, fullName}
       in Right Contact {contactId, localDisplayName, profile, activeConn = undefined}
    toContact _ = Left $ SEContactNotFound localDisplayName
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEContactNotReady localDisplayName

getContactConnections :: StoreMonad m => SQLiteStore -> UserId -> ContactName -> m [Connection]
getContactConnections st userId displayName =
  liftIOEither . withTransaction st $ \db ->
    connections
      <$> DB.queryNamed
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
          FROM connections c
          JOIN contacts cs ON c.contact_id == cs.contact_id
          WHERE c.user_id = :user_id
            AND cs.user_id = :user_id
            AND cs.local_display_name == :display_name
        |]
        [":user_id" := userId, ":display_name" := displayName]
  where
    connections [] = Left $ SEContactNotFound displayName
    connections rows = Right $ map toConnection rows

type ConnectionRow = (Int64, ConnId, Int, Maybe Int64, ConnStatus, ConnType, Maybe Int64, Maybe Int64, UTCTime)

type MaybeConnectionRow = (Maybe Int64, Maybe ConnId, Maybe Int, Maybe Int64, Maybe ConnStatus, Maybe ConnType, Maybe Int64, Maybe Int64, Maybe UTCTime)

toConnection :: ConnectionRow -> Connection
toConnection (connId, agentConnId, connLevel, viaContact, connStatus, connType, contactId, groupMemberId, createdAt) =
  let entityId = entityId_ connType
   in Connection {connId, agentConnId, connLevel, viaContact, connStatus, connType, entityId, createdAt}
  where
    entityId_ :: ConnType -> Maybe Int64
    entityId_ ConnContact = contactId
    entityId_ ConnMember = groupMemberId

toMaybeConnection :: MaybeConnectionRow -> Maybe Connection
toMaybeConnection (Just connId, Just agentConnId, Just connLevel, viaContact, Just connStatus, Just connType, contactId, groupMemberId, Just createdAt) =
  Just $ toConnection (connId, agentConnId, connLevel, viaContact, connStatus, connType, contactId, groupMemberId, createdAt)
toMaybeConnection _ = Nothing

getConnectionChatDirection :: StoreMonad m => SQLiteStore -> User -> ConnId -> m (ChatDirection 'Agent)
getConnectionChatDirection st User {userId, userContactId} agentConnId =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    c@Connection {connType, entityId} <- getConnection_ db
    case connType of
      ConnMember ->
        case entityId of
          Nothing -> throwError $ SEInternal "group member without connection"
          Just groupMemberId -> uncurry (ReceivedGroupMessage c) <$> getGroupAndMember_ db groupMemberId c
      ConnContact ->
        case entityId of
          Nothing -> pure $ ReceivedDMConnection c
          Just contactId -> ReceivedDMContact <$> getContact_ db contactId c
  where
    getConnection_ :: DB.Connection -> ExceptT StoreError IO Connection
    getConnection_ db = ExceptT $ do
      connection
        <$> DB.query
          db
          [sql|
            SELECT connection_id, agent_conn_id, conn_level, via_contact,
              conn_status, conn_type, contact_id, group_member_id, created_at
            FROM connections
            WHERE user_id = ? AND agent_conn_id = ?
          |]
          (userId, agentConnId)
    connection :: [ConnectionRow] -> Either StoreError Connection
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEConnectionNotFound agentConnId
    getContact_ :: DB.Connection -> Int64 -> Connection -> ExceptT StoreError IO Contact
    getContact_ db contactId c = ExceptT $ do
      toContact contactId c
        <$> DB.query
          db
          [sql|
            SELECT c.local_display_name, p.display_name, p.full_name
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.contact_id = ?
          |]
          (userId, contactId)
    toContact :: Int64 -> Connection -> [(ContactName, Text, Text)] -> Either StoreError Contact
    toContact contactId activeConn [(localDisplayName, displayName, fullName)] =
      let profile = Profile {displayName, fullName}
       in Right $ Contact {contactId, localDisplayName, profile, activeConn}
    toContact _ _ _ = Left $ SEInternal "referenced contact not found"
    getGroupAndMember_ :: DB.Connection -> Int64 -> Connection -> ExceptT StoreError IO (GroupName, GroupMember)
    getGroupAndMember_ db groupMemberId c = ExceptT $ do
      toGroupAndMember c
        <$> DB.query
          db
          [sql|
            SELECT
              g.local_display_name,
              m.group_member_id, m.member_id, m.member_role, m.member_category, m.member_status,
              m.invited_by, m.local_display_name, m.contact_id, p.display_name, p.full_name
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            JOIN groups g ON g.group_id = m.group_id
            WHERE m.group_member_id = ?
          |]
          (Only groupMemberId)
    toGroupAndMember :: Connection -> [Only GroupName :. GroupMemberRow] -> Either StoreError (GroupName, GroupMember)
    toGroupAndMember c [Only groupName :. memberRow] =
      let member = toGroupMember userContactId memberRow
       in Right (groupName, (member :: GroupMember) {activeConn = Just c})
    toGroupAndMember _ _ = Left $ SEInternal "referenced group member not found"

updateConnectionStatus :: MonadUnliftIO m => SQLiteStore -> Connection -> ConnStatus -> m ()
updateConnectionStatus st Connection {connId} connStatus =
  liftIO . withTransaction st $ \db ->
    DB.execute db "UPDATE connections SET conn_status = ? WHERE connection_id = ?" (connId, connStatus)

-- | creates completely new group with a single member - the current user
createNewGroup :: StoreMonad m => SQLiteStore -> TVar ChaChaDRG -> User -> GroupProfile -> m Group
createNewGroup st gVar user groupProfile =
  liftIOEither . checkConstraint SEDuplicateName . withTransaction st $ \db -> do
    let GroupProfile {displayName, fullName} = groupProfile
        uId = userId user
    DB.execute db "INSERT INTO display_names (local_display_name, ldn_base, user_id) VALUES (?, ?, ?)" (displayName, displayName, uId)
    DB.execute db "INSERT INTO group_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
    profileId <- insertedRowId db
    DB.execute db "INSERT INTO groups (local_display_name, user_id, group_profile_id) VALUES (?, ?, ?)" (displayName, uId, profileId)
    groupId <- insertedRowId db
    memberId <- randomId gVar 12
    membership <- createContactMember_ db user groupId user (memberId, GROwner) GCUserMember GSMemCreator IBUser
    pure $ Right Group {groupId, localDisplayName = displayName, groupProfile, members = [], membership}

-- | creates a new group record for the group the current user was invited to
createGroupInvitation ::
  StoreMonad m => SQLiteStore -> User -> Contact -> GroupInvitation -> m Group
createGroupInvitation st user contact GroupInvitation {fromMember, invitedMember, queueInfo, groupProfile} =
  liftIOEither . withTransaction st $ \db -> do
    let GroupProfile {displayName, fullName} = groupProfile
        uId = userId user
    withLocalDisplayName db uId displayName $ \localDisplayName -> do
      DB.execute db "INSERT INTO group_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
      profileId <- insertedRowId db
      DB.execute db "INSERT INTO groups (group_profile_id, local_display_name, inv_queue_info, user_id) VALUES (?, ?, ?, ?)" (profileId, localDisplayName, queueInfo, uId)
      groupId <- insertedRowId db
      member <- createContactMember_ db user groupId contact fromMember GCHostMember GSMemInvited IBUnknown
      membership <- createContactMember_ db user groupId user invitedMember GCUserMember GSMemInvited (IBContact $ contactId contact)
      pure Group {groupId, localDisplayName, groupProfile, members = [member], membership}

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getGroup :: StoreMonad m => SQLiteStore -> User -> GroupName -> m Group
getGroup st user localDisplayName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ fst <$> getGroup_ db user localDisplayName

getGroup_ :: DB.Connection -> User -> GroupName -> ExceptT StoreError IO (Group, Maybe SMPQueueInfo)
getGroup_ db User {userId, userContactId} localDisplayName = do
  (g@Group {groupId}, qInfo) <- getGroupRec_
  allMembers <- getMembers_ groupId
  (members, membership) <- liftEither $ splitUserMember_ allMembers
  pure (g {members, membership}, qInfo)
  where
    getGroupRec_ :: ExceptT StoreError IO (Group, Maybe SMPQueueInfo)
    getGroupRec_ = ExceptT $ do
      toGroup
        <$> DB.query
          db
          [sql|
            SELECT g.group_id, p.display_name, p.full_name, g.inv_queue_info
            FROM groups g
            JOIN group_profiles p ON p.group_profile_id = g.group_profile_id
            WHERE g.local_display_name = ? AND g.user_id = ?
          |]
          (localDisplayName, userId)
    toGroup :: [(Int64, GroupName, Text, Maybe SMPQueueInfo)] -> Either StoreError (Group, Maybe SMPQueueInfo)
    toGroup [(groupId, displayName, fullName, qInfo)] =
      let groupProfile = GroupProfile {displayName, fullName}
       in Right (Group {groupId, localDisplayName, groupProfile, members = undefined, membership = undefined}, qInfo)
    toGroup _ = Left $ SEGroupNotFound localDisplayName
    getMembers_ :: Int64 -> ExceptT StoreError IO [GroupMember]
    getMembers_ groupId = ExceptT $ do
      Right . map toContactMember
        <$> DB.query
          db
          [sql|
            SELECT
              m.group_member_id, m.member_id, m.member_role, m.member_category, m.member_status,
              m.invited_by, m.local_display_name, m.contact_id, p.display_name, p.full_name,
              c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
              c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            LEFT JOIN connections c ON c.connection_id = (
              SELECT max(cc.connection_id)
              FROM connections cc
              where cc.group_member_id = m.group_member_id
            )
            WHERE m.group_id = ? AND m.user_id = ?
          |]
          (groupId, userId)
    toContactMember :: (GroupMemberRow :. MaybeConnectionRow) -> GroupMember
    toContactMember (memberRow :. connRow) =
      (toGroupMember userContactId memberRow) {activeConn = toMaybeConnection connRow}
    splitUserMember_ :: [GroupMember] -> Either StoreError ([GroupMember], GroupMember)
    splitUserMember_ allMembers =
      let (b, a) = break ((== Just userContactId) . memberContactId) allMembers
       in case a of
            [] -> Left SEGroupWithoutUser
            u : ms -> Right (b <> ms, u)

getGroupInvitation :: StoreMonad m => SQLiteStore -> User -> GroupName -> m ReceivedGroupInvitation
getGroupInvitation st user localDisplayName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    (Group {membership, members, groupProfile}, qInfo) <- getGroup_ db user localDisplayName
    when (memberStatus membership /= GSMemInvited) $ throwError SEGroupAlreadyJoined
    case (qInfo, findFromContact (invitedBy membership) members) of
      (Just queueInfo, Just fromMember) ->
        pure ReceivedGroupInvitation {fromMember, userMember = membership, queueInfo, groupProfile}
      _ -> throwError SEGroupInvitationNotFound
  where
    findFromContact :: InvitedBy -> [GroupMember] -> Maybe GroupMember
    findFromContact (IBContact contactId) = find ((== Just contactId) . memberContactId)
    findFromContact _ = const Nothing

type GroupMemberRow = (Int64, ByteString, GroupMemberRole, GroupMemberCategory, GroupMemberStatus, Maybe Int64, ContactName, Maybe Int64, ContactName, Text)

toGroupMember :: Int64 -> GroupMemberRow -> GroupMember
toGroupMember userContactId (groupMemberId, memberId, memberRole, memberCategory, memberStatus, invitedById, localDisplayName, memberContactId, displayName, fullName) =
  let memberProfile = Profile {displayName, fullName}
      invitedBy = toInvitedBy userContactId invitedById
   in GroupMember {groupMemberId, memberId, memberRole, memberCategory, memberStatus, invitedBy, localDisplayName, memberProfile, memberContactId, activeConn = Nothing}

createContactGroupMember :: StoreMonad m => SQLiteStore -> TVar ChaChaDRG -> User -> Int64 -> Contact -> GroupMemberRole -> ConnId -> m GroupMember
createContactGroupMember st gVar user groupId contact memberRole agentConnId =
  liftIOEither . withTransaction st $ \db ->
    createWithRandomId gVar $ \memId -> do
      member <- createContactMember_ db user groupId contact (memId, memberRole) GCInviteeMember GSMemInvited IBUser
      groupMemberId <- insertedRowId db
      void $ createMemberConnection_ db (userId user) groupMemberId agentConnId Nothing 0
      pure member

createMemberConnection :: MonadUnliftIO m => SQLiteStore -> UserId -> Int64 -> ConnId -> m ()
createMemberConnection st userId groupMemberId agentConnId =
  liftIO . withTransaction st $ \db ->
    void $ createMemberConnection_ db userId groupMemberId agentConnId Nothing 0

updateGroupMemberStatus :: MonadUnliftIO m => SQLiteStore -> UserId -> Int64 -> GroupMemberStatus -> m ()
updateGroupMemberStatus st userId groupMemberId memberStatus =
  liftIO . withTransaction st $ \db ->
    DB.executeNamed
      db
      [sql|
        UPDATE group_members
        SET member_status = :member_status
        WHERE user_id = :user_id AND group_member_id = :group_member_id
      |]
      [ ":user_id" := userId,
        ":group_member_id" := groupMemberId,
        ":member_status" := memberStatus
      ]

-- | add new member with profile
createNewGroupMember :: StoreMonad m => SQLiteStore -> User -> Group -> MemberInfo -> GroupMemberCategory -> GroupMemberStatus -> m GroupMember
createNewGroupMember st user@User {userId} group memInfo@(MemberInfo _ _ Profile {displayName, fullName}) memCategory memStatus =
  liftIOEither . withTransaction st $ \db ->
    withLocalDisplayName db userId displayName $ \localDisplayName -> do
      DB.execute db "INSERT INTO contact_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
      memProfileId <- insertedRowId db
      let newMember =
            NewGroupMember
              { memInfo,
                memCategory,
                memStatus,
                memInvitedBy = IBUnknown,
                localDisplayName,
                memContactId = Nothing,
                memProfileId
              }
      createNewMember_ db user group newMember

createNewMember_ :: DB.Connection -> User -> Group -> NewGroupMember -> IO GroupMember
createNewMember_
  db
  User {userId, userContactId}
  Group {groupId}
  NewGroupMember
    { memInfo = MemberInfo memberId memberRole memberProfile,
      memCategory = memberCategory,
      memStatus = memberStatus,
      memInvitedBy = invitedBy,
      localDisplayName,
      memContactId = memberContactId,
      memProfileId
    } = do
    let invitedById = fromInvitedBy userContactId invitedBy
    DB.execute
      db
      [sql|
        INSERT INTO group_members
          (group_id, member_id, member_role, member_category, member_status,
           invited_by, user_id, local_display_name, contact_profile_id, contact_id) VALUES (?,?,?,?,?,?,?,?,?,?)
      |]
      (groupId, memberId, memberRole, memberCategory, memberStatus, invitedById, userId, localDisplayName, memProfileId, memberContactId)
    groupMemberId <- insertedRowId db
    pure $
      GroupMember
        { groupMemberId,
          memberId,
          memberRole,
          memberStatus,
          memberCategory,
          invitedBy,
          memberProfile,
          localDisplayName,
          memberContactId,
          activeConn = Nothing
        }

createIntroductions :: MonadUnliftIO m => SQLiteStore -> Group -> GroupMember -> m [GroupMemberIntro]
createIntroductions st Group {members} toMember = do
  let reMembers = filter (\m -> memberCurrent m && groupMemberId m /= groupMemberId toMember) members
  if null reMembers
    then pure []
    else liftIO . withTransaction st $ \db ->
      mapM (insertIntro_ db) reMembers
  where
    insertIntro_ :: DB.Connection -> GroupMember -> IO GroupMemberIntro
    insertIntro_ db reMember = do
      DB.execute
        db
        [sql|
          INSERT INTO group_member_intros
            (re_group_member_id, to_group_member_id, intro_status) VALUES (?,?,?)
        |]
        (groupMemberId reMember, groupMemberId toMember, GMIntroPending)
      introId <- insertedRowId db
      pure GroupMemberIntro {introId, reMember, toMember, introStatus = GMIntroPending, introInvitation = Nothing}

updateIntroStatus :: MonadUnliftIO m => SQLiteStore -> GroupMemberIntro -> GroupMemberIntroStatus -> m ()
updateIntroStatus st GroupMemberIntro {introId} introStatus' =
  liftIO . withTransaction st $ \db ->
    DB.executeNamed
      db
      [sql|
        UPDATE group_member_intros
        SET intro_status = :intro_status
        WHERE group_member_intro_id = :intro_id
      |]
      [":intro_status" := introStatus', ":intro_id" := introId]

saveIntroInvitation :: StoreMonad m => SQLiteStore -> GroupMember -> GroupMember -> IntroInvitation -> m GroupMemberIntro
saveIntroInvitation st reMember toMember introInv = do
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    intro <- getIntroduction_ db reMember toMember
    liftIO $
      DB.executeNamed
        db
        [sql|
          UPDATE group_member_intros
          SET intro_status = :intro_status,
              group_queue_info = :group_queue_info,
              direct_queue_info = :direct_queue_info
          WHERE group_member_intro_id = :intro_id
        |]
        [ ":intro_status" := GMIntroInvReceived,
          ":group_queue_info" := groupQInfo introInv,
          ":direct_queue_info" := directQInfo introInv,
          ":intro_id" := introId intro
        ]
    pure intro {introInvitation = Just introInv, introStatus = GMIntroInvReceived}

saveMemberInvitation :: StoreMonad m => SQLiteStore -> GroupMember -> IntroInvitation -> m ()
saveMemberInvitation st GroupMember {groupMemberId} IntroInvitation {groupQInfo, directQInfo} =
  liftIO . withTransaction st $ \db ->
    DB.executeNamed
      db
      [sql|
        UPDATE group_members
        SET member_status = :member_status,
            group_queue_info = :group_queue_info,
            direct_queue_info = :direct_queue_info
        WHERE group_member_id = :group_member_id
      |]
      [ ":member_status" := GSMemIntroInvited,
        ":group_queue_info" := groupQInfo,
        ":direct_queue_info" := directQInfo,
        ":group_member_id" := groupMemberId
      ]

getIntroduction_ :: DB.Connection -> GroupMember -> GroupMember -> ExceptT StoreError IO GroupMemberIntro
getIntroduction_ db reMember toMember = ExceptT $ do
  toIntro
    <$> DB.query
      db
      [sql|
        SELECT group_member_intro_id, group_queue_info, direct_queue_info, intro_status
        FROM group_member_intros
        WHERE re_group_member_id = ? AND to_group_member_id = ?
      |]
      (groupMemberId reMember, groupMemberId toMember)
  where
    toIntro :: [(Int64, Maybe SMPQueueInfo, Maybe SMPQueueInfo, GroupMemberIntroStatus)] -> Either StoreError GroupMemberIntro
    toIntro [(introId, groupQInfo, directQInfo, introStatus)] =
      let introInvitation = IntroInvitation <$> groupQInfo <*> directQInfo
       in Right GroupMemberIntro {introId, reMember, toMember, introStatus, introInvitation}
    toIntro _ = Left SEIntroNotFound

createIntroReMember :: StoreMonad m => SQLiteStore -> User -> Group -> GroupMember -> MemberInfo -> ConnId -> ConnId -> m GroupMember
createIntroReMember st user@User {userId} group _host@GroupMember {memberContactId, activeConn} memInfo@(MemberInfo _ _ memberProfile) groupAgentConnId directAgentConnId =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    let cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
    Connection {connId = directConnId} <- liftIO $ createConnection_ db userId directAgentConnId memberContactId cLevel
    (localDisplayName, contactId, memProfileId) <- ExceptT $ createContact_ db userId directConnId memberProfile
    liftIO $ do
      let newMember =
            NewGroupMember
              { memInfo,
                memCategory = GCPreMember,
                memStatus = GSMemIntroduced,
                memInvitedBy = IBUnknown,
                localDisplayName,
                memContactId = Just contactId,
                memProfileId
              }
      member <- createNewMember_ db user group newMember
      conn <- createMemberConnection_ db userId (groupMemberId member) groupAgentConnId memberContactId cLevel
      pure (member :: GroupMember) {activeConn = Just conn}

createIntroToMemberContact :: StoreMonad m => SQLiteStore -> UserId -> GroupMember -> GroupMember -> ConnId -> ConnId -> m ()
createIntroToMemberContact st userId GroupMember {memberContactId = viaContactId, activeConn} _to@GroupMember {groupMemberId, localDisplayName} groupAgentConnId directAgentConnId =
  liftIO . withTransaction st $ \db -> do
    let cLevel = 1 + maybe 0 (connLevel :: Connection -> Int) activeConn
    void $ createMemberConnection_ db userId groupMemberId groupAgentConnId viaContactId cLevel
    Connection {connId = directConnId} <- createConnection_ db userId directAgentConnId viaContactId cLevel
    contactId <- createMemberContact_ db directConnId
    updateMember_ db contactId
  where
    createMemberContact_ :: DB.Connection -> Int64 -> IO Int64
    createMemberContact_ db connId = do
      DB.executeNamed
        db
        [sql|
          INSERT INTO contacts (contact_profile_id, local_display_name, user_id)
          VALUES ((
            SELECT contact_profile_id
            FROM group_members
            WHERE group_member_id = :group_member_id
          ), :local_display_name, :user_id)
        |]
        [ ":group_member_id" := groupMemberId,
          ":local_display_name" := localDisplayName,
          ":user_id" := userId
        ]
      contactId <- insertedRowId db
      DB.execute db "UPDATE connections SET contact_id = ? WHERE connection_id = ?" (contactId, connId)
      pure contactId
    updateMember_ :: DB.Connection -> Int64 -> IO ()
    updateMember_ db contactId =
      DB.executeNamed
        db
        [sql|
          UPDATE group_members
          SET contact_id = :contact_id
          WHERE group_member_id = :group_member_id
        |]
        [":contact_id" := contactId, ":group_member_id" := groupMemberId]

createMemberConnection_ :: DB.Connection -> UserId -> Int64 -> ConnId -> Maybe Int64 -> Int -> IO Connection
createMemberConnection_ db userId groupMemberId agentConnId viaContact connLevel = do
  createdAt <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO connections
        (user_id, agent_conn_id, conn_status, conn_type, group_member_id, via_contact, conn_level, created_at) VALUES (?,?,?,?,?,?,?,?);
    |]
    (userId, agentConnId, ConnNew, ConnMember, groupMemberId, viaContact, connLevel, createdAt)
  connId <- insertedRowId db
  pure Connection {connId, agentConnId, connType = ConnMember, entityId = Just groupMemberId, viaContact, connLevel, connStatus = ConnNew, createdAt}

createContactMember_ :: IsContact a => DB.Connection -> User -> Int64 -> a -> (MemberId, GroupMemberRole) -> GroupMemberCategory -> GroupMemberStatus -> InvitedBy -> IO GroupMember
createContactMember_ db User {userId, userContactId} groupId userOrContact (memberId, memberRole) memberCategory memberStatus invitedBy = do
  insertMember_
  groupMemberId <- insertedRowId db
  let memberProfile = profile' userOrContact
      memberContactId = Just $ contactId' userOrContact
      localDisplayName = localDisplayName' userOrContact
  pure GroupMember {groupMemberId, memberId, memberRole, memberCategory, memberStatus, invitedBy, localDisplayName, memberProfile, memberContactId, activeConn = Nothing}
  where
    insertMember_ =
      DB.executeNamed
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_category, member_status, invited_by,
              user_id, local_display_name, contact_profile_id, contact_id)
          VALUES
            (:group_id,:member_id,:member_role,:member_category,:member_status,:invited_by,
             :user_id,:local_display_name,
              (SELECT contact_profile_id FROM contacts WHERE contact_id = :contact_id),
              :contact_id)
        |]
        [ ":group_id" := groupId,
          ":member_id" := memberId,
          ":member_role" := memberRole,
          ":member_category" := memberCategory,
          ":member_status" := memberStatus,
          ":invited_by" := fromInvitedBy userContactId invitedBy,
          ":user_id" := userId,
          ":local_display_name" := localDisplayName' userOrContact,
          ":contact_id" := contactId' userOrContact
        ]

-- | Saves unique local display name based on passed displayName, suffixed with _N if required.
-- This function should be called inside transaction.
withLocalDisplayName :: forall a. DB.Connection -> UserId -> Text -> (Text -> IO a) -> IO (Either StoreError a)
withLocalDisplayName db userId displayName action = getLdnSuffix >>= (`tryCreateName` 20)
  where
    getLdnSuffix :: IO Int
    getLdnSuffix =
      maybe 0 ((+ 1) . fromOnly) . listToMaybe
        <$> DB.queryNamed
          db
          [sql|
            SELECT ldn_suffix FROM display_names
            WHERE user_id = :user_id AND ldn_base = :display_name
            ORDER BY ldn_suffix DESC
            LIMIT 1
          |]
          [":user_id" := userId, ":display_name" := displayName]
    tryCreateName :: Int -> Int -> IO (Either StoreError a)
    tryCreateName _ 0 = pure $ Left SEDuplicateName
    tryCreateName ldnSuffix attempts = do
      let ldn = displayName <> (if ldnSuffix == 0 then "" else T.pack $ '_' : show ldnSuffix)
      E.try (insertName ldn) >>= \case
        Right () -> Right <$> action ldn
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreateName (ldnSuffix + 1) (attempts - 1)
          | otherwise -> E.throwIO e
      where
        insertName ldn =
          DB.execute
            db
            [sql|
              INSERT INTO display_names
                (local_display_name, ldn_base, ldn_suffix, user_id) VALUES (?, ?, ?, ?)
            |]
            (ldn, displayName, ldnSuffix, userId)

createWithRandomId :: forall a. TVar ChaChaDRG -> (ByteString -> IO a) -> IO (Either StoreError a)
createWithRandomId gVar create = tryCreate 3
  where
    tryCreate :: Int -> IO (Either StoreError a)
    tryCreate 0 = pure $ Left SEUniqueID
    tryCreate n = do
      id' <- randomId gVar 12
      E.try (create id') >>= \case
        Right x -> pure $ Right x
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreate (n - 1)
          | otherwise -> pure . Left . SEInternal $ bshow e

randomId :: TVar ChaChaDRG -> Int -> IO ByteString
randomId gVar n = B64.encode <$> (atomically . stateTVar gVar $ randomBytesGenerate n)

data StoreError
  = SEDuplicateName
  | SEContactNotFound ContactName
  | SEContactNotReady ContactName
  | SEGroupNotFound GroupName
  | SEGroupWithoutUser
  | SEDuplicateGroupMember
  | SEGroupAlreadyJoined
  | SEGroupInvitationNotFound
  | SEConnectionNotFound ConnId
  | SEIntroNotFound
  | SEUniqueID
  | SEInternal ByteString
  deriving (Show, Exception)
