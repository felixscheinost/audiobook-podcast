{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
    bookCover,
    formatDataFullPath,
    module Database.Calibre.Format,
    module Database.Calibre.Tables,
    module Database.Calibre.Queries
) where

import qualified Data.Text                as T
import           Database.Beam
import           Database.Calibre.Format  (bookFormatFileExtension)
import           Database.Calibre.Queries
import           Database.Calibre.Tables
import           Import.NoFoundation

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
