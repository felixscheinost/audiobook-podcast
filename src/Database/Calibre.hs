{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
    bookCover,
    bookFullPath,
    BookAndData(..),
    module Database.Calibre.BookFormat,
    module Database.Calibre.Tables,
    module Database.Calibre.Queries
) where

import qualified Data.Text                   as T
import           Database.Beam
import           Database.Calibre.BookFormat (toFileExtension)
import           Database.Calibre.Queries
import           Database.Calibre.Tables
import           Import
import           System.FilePath             (replaceFileName, (<.>), (</>))

libraryPath :: AppSettings -> FilePath
libraryPath o = appCalibreLibraryFolder o </> "metadata.db"

bookFullPath :: BookAndData -> Handler FilePath
bookFullPath BookAndData{..} = do
    app <- getYesod
    return $ appCalibreLibraryFolder (appSettings app)
        </> T.unpack (bookPath bdBook)
        </> T.unpack (dataName bdData)
        <.> T.unpack (toFileExtension $ dataFormat bdData)

bookCover :: BookAndData -> Handler FilePath
bookCover ab = (`replaceFileName` "cover.jpg") <$> bookFullPath ab
