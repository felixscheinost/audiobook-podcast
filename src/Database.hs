
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database (
    RunSQL(..),
    runSQL,
    module Database.Tables,
    module Database.Queries,
) where

import qualified Data.Text              as T
import           Database.Beam
import           Database.Queries
import           Database.SQLite.Simple as Sql
import           Database.Tables
import           Import.NoFoundation

class MonadIO m => RunSQL m where
    dbConnection :: m (MVar Sql.Connection)

-- | Tries to run the given function with the Connection from the MVar
-- If there is no connection, tries to establish one.
-- If establishing a connection fails `SqlErrorWhileConnecting` is thrown
runSQL :: (ReadSettings m, RunSQL m) => (Sql.Connection -> IO a) -> m a
runSQL f = do
    mConn <- dbConnection
    liftIO $ withMVar mConn f
