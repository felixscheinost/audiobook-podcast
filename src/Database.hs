
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database (
    RunSQL(..),
    runSQL,
    insertAudiobook,
    module Database.Tables,
    module Database.Queries,
) where

import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Queries
import           Database.SQLite.Simple as Sql
import           Database.Tables
import           Import.NoFoundation

insertAudiobook :: Audiobook -> Connection -> IO ()
insertAudiobook ab conn = runBeamSqlite conn  $ runInsert $
    insert (dbAudiobooks db) $
        insertExpressions [
            Audiobook
                default_
                (val_ $ abPath ab)
                (val_ $ abTitle ab)
                (val_ $ abAuthor ab)
                (val_ $ abSeries ab)
                (val_ $ abSeriesIndex ab)
        ]

class MonadIO m => RunSQL m where
    dbConnection :: m (MVar Sql.Connection)

-- | Tries to run the given function with the Connection from the MVar
-- If there is no connection, tries to establish one.
-- If establishing a connection fails `SqlErrorWhileConnecting` is thrown
runSQL :: RunSQL m => (Sql.Connection -> IO a) -> m a
runSQL f = do
    mConn <- dbConnection
    liftIO $ withMVar mConn f
