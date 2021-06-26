{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Store where

import Control.Concurrent.STM
import Control.Monad (replicateM_)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text.Encoding (decodeUtf8)
import qualified Database.SQLite.Simple as DB
import Numeric.Natural (Natural)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), connectSQLiteStore, createSQLiteStore)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import System.FilePath (takeBaseName, takeExtension)

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations =
  sortBy (compare `on` name) . map migration . filter sqlFile $
    $(makeRelativeToProject "migrations" >>= embedDir)
  where
    sqlFile (file, _) = takeExtension file == ".sql"
    migration (file, qStr) = Migration {name = takeBaseName file, up = decodeUtf8 qStr}

data SQLitePool = SQLitePool
  { dbFilePath :: FilePath,
    dbPool :: TBQueue DB.Connection,
    dbNew :: Bool
  }

createStore :: FilePath -> Natural -> IO SQLitePool
createStore dbFilePath poolSize = do
  SQLiteStore {dbConn = c, dbNew} <- createSQLiteStore dbFilePath migrations
  dbPool <- newTBQueueIO poolSize
  atomically $ writeTBQueue dbPool c
  replicateM_ (fromInteger $ toInteger $ poolSize - 1) $
    connectSQLiteStore dbFilePath >>= atomically . writeTBQueue dbPool . dbConn
  pure SQLitePool {dbFilePath, dbPool, dbNew}
