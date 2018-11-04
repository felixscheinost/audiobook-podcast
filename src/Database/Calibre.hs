{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
    bookCover,
    bookFullPath,
    BookAndData,
    module Database.Calibre.BookFormat,
    module Database.Calibre.Tables,
    module Database.Calibre.Queries
) where

import qualified Data.Text                   as T
import           Database.Beam
import           Database.Calibre.BookFormat (bookFormatFileExtension)
import           Database.Calibre.Queries
import           Database.Calibre.Tables
import           Import
import           System.FilePath             (replaceFileName, (<.>), (</>))

libraryPath :: AppSettings -> FilePath
libraryPath o = appCalibreLibraryFolder o </> "metadata.db"

bookFullPath :: BookAndData -> Handler FilePath
bookFullPath (book, bookData) = do
    app <- getYesod
    return $ appCalibreLibraryFolder (appSettings app)
        </> T.unpack (bookPath book)
        </> T.unpack (dataName bookData)
        <.> T.unpack (bookFormatFileExtension $ dataFormat bookData)

bookCover :: BookAndData -> Handler FilePath
bookCover bd = (`replaceFileName` "cover.jpg") <$> bookFullPath bd
