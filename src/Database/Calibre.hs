{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
    bookCover,
    bookFullPath,
    BookAndData,
    module Database.Calibre.Types,
    module Database.Calibre.Tables,
    module Database.Calibre.Queries
) where

import qualified Data.Text                as T
import           Database.Beam
import           Database.Calibre.Queries
import           Database.Calibre.Tables
import           Database.Calibre.Types
import           Import
import           System.FilePath          (replaceFileName, (<.>), (</>))

libraryPath :: AppSettings -> FilePath
libraryPath o = appCalibreLibraryFolder o </> "metadata.db"

type BookAndData = (CalibreBook, CalibreBookData)

bookFullPath :: BookAndData -> Handler FilePath
bookFullPath (book, bookData) = do
    app <- getYesod
    return $ appCalibreLibraryFolder (appSettings app)
        </> T.unpack (bookPath book)
        </> T.unpack (dataName bookData)
        <.> T.unpack (toFileExtension $ dataFormat bookData)

bookCover :: BookAndData -> Handler FilePath
bookCover ab = (`replaceFileName` "cover.jpg") <$> bookFullPath ab