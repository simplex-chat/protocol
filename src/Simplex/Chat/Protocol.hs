{-# LANGUAGE DuplicateRecordFields #-}

module Simplex.Chat.Protocol where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Simplex.Messaging.Agent.Protocol (ConnId)

data ChatEvent = GroupEvent | MessageEvent | InfoEvent

data Profile = Profile
  { profileId :: ByteString,
    displayName :: Text
  }

data Contact = Contact
  { contactId :: ByteString,
    profile :: Profile,
    connections :: [Connection]
  }

data Connection = Connection
  { connId :: ConnId,
    connLevel :: Int,
    viaConn :: ConnId
  }

data GroupMember = GroupMember
  { groupId :: ByteString,
    sharedMemberId :: ByteString,
    contact :: Contact,
    memberRole :: GroupMemberRole,
    memberStatus :: GroupMemberStatus
  }

data GroupMemberRole = GROwner | GRAdmin | GRStandard

data GroupMemberStatus = GSInvited | GSConnected | GSConnectedAll

data Group = Group
  { groupId :: ByteString,
    displayName :: Text,
    members :: [GroupMember]
  }
