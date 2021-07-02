{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Store
  ( SQLiteStore,
    StoreError (..),
    createStore,
    createDirectContact,
    getContactConnection,
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Data.ByteString.Char8 (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple (NamedParam (..), Only (..))
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (ConnId)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), createSQLiteStore, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import Simplex.Messaging.Util (liftIOEither)
import System.FilePath (takeBaseName, takeExtension)

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

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid();"

createDirectContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> Maybe Text -> m Contact'
createDirectContact st userId agentConnId contactRef =
  liftIO . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO connections (user_id, agent_conn_id) VALUES (?,?);" (userId, agentConnId)
    connId <- insertedRowId db
    let activeConn = Connection {connId, agentConnId, connLevel = 0, viaConn = Nothing, connStatus = ConnNew}
    -- TODO support undefined localContactRef (Nothing) - currently it would fail
    let localContactRef = fromMaybe "" contactRef
    DB.execute db "INSERT INTO contacts (user_id, local_contact_ref) VALUES (?,?);" (userId, localContactRef)
    contactId <- insertedRowId db
    DB.execute db "INSERT INTO contact_connections (connection_id, contact_id) VALUES (?,?);" (connId, contactId)
    pure Contact' {contactId, localContactRef, profile = Nothing, activeConn}

getContactConnection :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ContactRef -> m Connection
getContactConnection st userId contactRef =
  liftIOEither . withTransaction st $ \db -> do
    DB.queryNamed
      db
      [sql|
        SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_conn, c.conn_status
        FROM connections AS c
        JOIN contact_connections AS cc ON cc.connection_id == c.connection_id
        JOIN contacts AS cs ON cc.contact_id == cs.contact_id
        WHERE c.user_id = :user_id AND cs.user_id = :user_id AND cs.local_contact_ref == :contact_ref
        LIMIT 1;
      |]
      [":user_id" := userId, ":contact_ref" := contactRef]
      >>= \case
        [(connId, agentConnId, connLevel, viaConn, connStatus)] ->
          pure $ Right Connection {connId, agentConnId, connLevel, viaConn, connStatus}
        _ -> pure $ Left SEContactNotFound

-- getConnectionContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> Maybe Contact'
-- getConnectionContact st userId connId =
--   liftIO . withTransaction st $ \db -> do
--     DB.query
--       db
--       [sql|
--         SELECT c.contact_id, c.local_contact_ref, c.contact_status

--         connection_id INTEGER PRIMARY KEY,
--         agent_conn_id BLOB NOT NULL UNIQUE,
--         conn_level INTEGER NOT NULL DEFAULT 0,
--         via_conn BLOB REFERENCES contact_connections (connection_id),
--         conn_status TEXT NOT NULL DEFAULT '',
--         user_id INTEGER NOT NULL REFERENCES users

--         FROM contacts AS c
--         JOIN connections AS conns ON c.connection_id ==
--         JOIN contact_connections AS cc ON c.connection_id == conns.connection_id AND c.contact_id == cc.contact_id
--         WHERE conns.user_id = :user_id AND conns.connection_id = :connection_id
--         contact_profile_id INTEGER UNIQUE REFERENCES contact_profiles, -- profile sent by remote contact, NULL for incognito contacts
--         contact_status TEXT NOT NULL DEFAULT '',
--         user_id
--       |]

data StoreError = SEContactNotFound | SEInternal ByteString
  deriving (Show, Exception)
