CREATE TABLE contact_profiles ( -- remote user profile
  contact_profile_id INTEGER PRIMARY KEY,
  contact_ref TEXT NOT NULL, -- contact name set by remote user (not unique), this name must not contain spaces
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with contact profile properties
);

-- the first record (id = 1) is reserved for the first local user
INSERT INTO contact_profiles (contact_profile_id, contact_ref) VALUES (1, '');


CREATE TABLE users (
  user_id INTEGER PRIMARY KEY,
  contact_profile_id INTEGER NOT NULL UNIQUE REFERENCES contact_profiles -- user's profile
);

-- the first record (id = 1) is reserved for the first local user
INSERT INTO users (user_id, contact_profile_id) VALUES (1, 1);

CREATE TABLE known_servers(
  server_id INTEGER PRIMARY KEY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB,
  user_id INTEGER NOT NULL REFERENCES user_id,
  UNIQUE (user_id, host, port)
) WITHOUT ROWID;

CREATE TABLE contacts (
  contact_id INTEGER PRIMARY KEY,
  local_contact_ref TEXT NOT NULL UNIQUE, -- contact name set by local user - must be unique
  local_properties TEXT NOT NULL DEFAULT '{}', -- JSON set by local user
  contact_profile_id INTEGER UNIQUE REFERENCES contact_profiles, -- profile sent by remote contact, NULL for incognito contacts
  contact_status TEXT NOT NULL DEFAULT '',
  user_id INTEGER NOT NULL REFERENCES user_id
);

CREATE TABLE connections ( -- all SMP agent connections
  connection_id INTEGER PRIMARY KEY,
  agent_conn_id BLOB NOT NULL UNIQUE,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_conn BLOB REFERENCES contact_connections (connection_id),
  conn_status TEXT NOT NULL DEFAULT '',
  user_id INTEGER NOT NULL REFERENCES user_id
);

CREATE TABLE contact_connections ( -- connections only for direct messages, many per contact
  connection_id INTEGER NOT NULL UNIQUE REFERENCES connections ON DELETE CASCADE,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT -- connection must be removed first via the agent
);

CREATE TABLE contact_invitations (
  invitation_id INTEGER PRIMARY KEY,
  agent_inv_id BLOB UNIQUE,
  invitation TEXT,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  invitation_status TEXT NOT NULL DEFAULT ''
);

CREATE TABLE group_profiles ( -- shared group profiles
  group_profile_id INTEGER PRIMARY KEY,
  group_ref TEXT NOT NULL, -- this name must not contain spaces
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with user or contact profile
);

CREATE TABLE groups (
  group_id INTEGER PRIMARY KEY, -- local group ID
  local_group_ref TEXT NOT NULL UNIQUE, -- local group name without spaces
  local_properties TEXT NOT NULL, -- local JSON group properties
  group_profile_id INTEGER REFERENCES group_profiles, -- shared group profile
  user_group_member_details_id INTEGER NOT NULL
    REFERENCES group_member_details (group_member_details_id) ON DELETE RESTRICT,
  user_id INTEGER NOT NULL REFERENCES user_id
);

CREATE TABLE group_members ( -- group members, excluding the local user
  group_member_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  group_member_details_id INTEGER NOT NULL REFERENCES group_member_details ON DELETE RESTRICT,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  connection_id INTEGER UNIQUE REFERENCES connections
);

CREATE TABLE group_member_details (
  group_member_details_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  member_id BLOB NOT NULL, -- shared member ID, unique per group
  member_role TEXT NOT NULL DEFAULT '', -- owner, admin, moderator, ''
  member_status TEXT NOT NULL DEFAULT '', -- inv | con | full | off
  invited_by INTEGER REFERENCES contacts ON DELETE RESTRICT, -- NULL for the members who joined before the current user and for the group creator
  UNIQUE (group_id, member_id)
);

CREATE TABLE events ( -- messages received by the agent, append only
  event_id INTEGER PRIMARY KEY,
  agent_msg_id INTEGER NOT NULL, -- internal message ID
  external_msg_id INTEGER NOT NULL, -- external message ID (sent or received)
  agent_meta TEXT NOT NULL, -- JSON with timestamps etc. sent in MSG
  connection_id INTEGER NOT NULL REFERENCES connections,
  received INTEGER NOT NULL, -- 0 for received, 1 for sent
  event_type TEXT NOT NULL, -- event type - see protocol/types.ts
  event_encoding INTEGER NOT NULL, -- format of event_body: 0 - binary, 1 - text utf8, 2 - JSON (utf8)
  content_type TEXT NOT NULL, -- content type - see protocol/types.ts
  event_body BLOB, -- agent message body as sent
  event_hash BLOB NOT NULL,
  integrity TEXT NOT NULL DEFAULT '',
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX events_external_msg_id_index ON events (connection_id, external_msg_id);

CREATE TABLE contact_profile_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  contact_profile_id INTEGER NOT NULL REFERENCES contact_profiles
);

CREATE TABLE group_profile_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  group_profile_id INTEGER NOT NULL REFERENCES group_profiles
);

CREATE TABLE group_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  group_member_id INTEGER REFERENCES group_members -- NULL for current user
);

CREATE TABLE group_event_parents (
  group_event_parent_id INTEGER PRIMARY KEY,
  event_id INTEGER NOT NULL REFERENCES group_events (event_id),
  parent_group_member_id INTEGER REFERENCES group_members (group_member_id), -- can be NULL if parent_member_id is incorrect
  parent_member_id BLOB, -- shared member ID, unique per group
  parent_event_id INTEGER REFERENCES events (event_id) ON DELETE CASCADE, -- this can be NULL if received event references another event that's not received yet
  parent_external_msg_id INTEGER NOT NULL,
  parent_event_hash BLOB NOT NULL
);

CREATE INDEX group_event_parents_parent_external_msg_id_index
  ON group_event_parents (parent_member_id, parent_external_msg_id);

CREATE TABLE blobs (
  blob_id INTEGER PRIMARY KEY,
  content BLOB NOT NULL
);

CREATE TABLE messages ( -- mutable messages presented to user
  message_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT, -- 1 for sent messages
  group_id INTEGER REFERENCES groups ON DELETE RESTRICT, -- NULL for direct messages
  deleted INTEGER NOT NULL, -- 1 for deleted
  msg_type TEXT NOT NULL,
  content_type TEXT NOT NULL,
  msg_text TEXT NOT NULL, -- textual representation
  msg_props TEXT NOT NULL, -- JSON
  msg_blob_id INTEGER REFERENCES blobs (blob_id) ON DELETE RESTRICT -- optional binary content
);

CREATE TABLE message_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  message_id INTEGER NOT NULL REFERENCES messages
);
