{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
    bookCover,
    formatDataFullPath,
    RunSQL(..),
    runSQL,
    module Database.Calibre.Format,
    module Database.Calibre.Tables,
    module Database.Calibre.Queries
) where

import qualified Data.Text                as T
import           Database.Beam
import           Database.Calibre.Format  (bookFormatFileExtension)
import           Database.Calibre.Queries
import           Database.Calibre.Tables
import           Database.SQLite.Simple   as Sql
import           Database.SQLite3         as Sql hiding (open)
import           Import.NoFoundation

class MonadIO m => RunSQL m where
    dbConnection :: m (MVar (Maybe Sql.Connection))

newtype SqlErrorWhileConnecting = SqlErrorWhileConnecting Sql.SQLError

instance Show SqlErrorWhileConnecting where
    show (SqlErrorWhileConnecting err) = "error connecting to DB: " ++ show err

instance Exception SqlErrorWhileConnecting

-- | Tries to run the given function with the Connection from the MVar
-- If there is no connection, tries to establish one.
-- If establishing a connection fails `SqlErrorWhileConnecting` is thrown
runSQL :: (ReadSettings m, RunSQL m) => (Sql.Connection -> IO a) -> m a
runSQL f = do
    mConn <- dbConnection
    settings <- asksSettings
    liftIO $ modifyMVar mConn $ \maybeConn -> do
            let newConn = handle (throwIO . SqlErrorWhileConnecting) $ Sql.open (libraryPath settings)
            conn <- maybe newConn return maybeConn
            res <- f conn
            return (Just conn, res)

libraryPath :: AppSettings -> FilePath
libraryPath o = appCalibreLibraryFolder o </> "metadata.db"

bookFolder :: ReadSettings m => CalibreBook -> m FilePath
bookFolder CalibreBook{bookPath=bookPath} = do
    calibreFolder <- appCalibreLibraryFolder <$> asksSettings
    return $ calibreFolder </> T.unpack bookPath

formatDataFullPath :: ReadSettings m => BookAndData -> m FilePath
formatDataFullPath (book, CalibreBookData{dataFormat=dataFormat, dataName=dataName}) = do
    folder <- bookFolder book
    return $ folder </> T.unpack dataName <.> T.unpack (bookFormatFileExtension dataFormat)

bookCover :: ReadSettings m => CalibreBook -> m FilePath
bookCover book = do
    folder <- bookFolder book
    return $ folder </> "cover.jpg"
