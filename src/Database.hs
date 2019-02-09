
{-# LANGUAGE TypeSynonymInstances #-}

module Database (
    RunSQL(..),
    runSQL,
    insertAudiobook,
    AppDbConnection,
    module Database.Tables,
    module Database.Queries,
) where

import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Queries
import           Database.SQLite.Simple as Sql
import           Database.Tables
import           Import.NoFoundation

type AppDbConnection = MVar Sql.Connection

insertAudiobook :: NewAudiobook -> Connection -> IO ()
insertAudiobook nab conn = runBeamSqlite conn  $ runInsert $
    insert (dbAudiobooks db) $
        insertExpressions [
            Audiobook
                default_
                (val_ $ nabPath nab)
                (val_ $ nabTitle nab)
                (val_ $ nabAuthor nab)
                (val_ $ nabSeries nab)
                (val_ $ nabSeriesIndex nab)
        ]

class MonadIO m => RunSQL m where
    dbConnection :: m AppDbConnection

-- | Tries to run the given function with the Connection from the MVar
-- If there is no connection, tries to establish one.
-- If establishing a connection fails `SqlErrorWhileConnecting` is thrown
runSQL :: RunSQL m => (Sql.Connection -> IO a) -> m a
runSQL f = do
    mConn <- dbConnection
    liftIO $ withMVar mConn f
