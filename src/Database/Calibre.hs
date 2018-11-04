{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
    bookCover,
    formatDataFullPath,
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

bookFolder :: CalibreBook -> Handler FilePath
bookFolder CalibreBook{bookPath=bookPath} = do
    calibreFolder <- appCalibreLibraryFolder . appSettings <$> getYesod
    return $ calibreFolder </> T.unpack bookPath

formatDataFullPath :: BookAndData -> Handler FilePath
formatDataFullPath (book, CalibreBookData{dataFormat=dataFormat, dataName=dataName}) = do
    folder <- bookFolder book
    return $ folder </> T.unpack dataName <.> T.unpack (bookFormatFileExtension dataFormat)

bookCover :: CalibreBook -> Handler FilePath
bookCover book = do
    folder <- bookFolder book
    return $ folder </> "cover.jpg"
