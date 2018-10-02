{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
    possibleAudiobookFormats,
    abCover,
    abTitle,
    module Database.Calibre.Types,
    module Database.Calibre.Tables,
    module Database.Calibre.Queries
) where

import qualified Data.Text                as T
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Calibre.Queries
import           Database.Calibre.Tables
import           Database.Calibre.Types
import           Import
import           System.FilePath          (replaceFileName, (<.>), (</>))

libraryPath :: AppSettings -> FilePath
libraryPath o = appCalibreLibraryFolder o </> "metadata.db"

possibleAudiobookFormats :: [AudiobookFormat]
possibleAudiobookFormats = ZIP : map SingleFile (enumFrom $ toEnum 0)

type Audiobook = (Book, Data)

abTitle :: Audiobook -> Text
abTitle = bookTitle . fst

abFullPath :: Audiobook -> Handler FilePath
abFullPath (book, bookData) = do
    app <- getYesod
    return $ appCalibreLibraryFolder (appSettings app)
        </> T.unpack (bookPath book)
        </> T.unpack (dataName bookData)
        <.> T.unpack (T.toLower $ dataFormat bookData)

abCover :: Audiobook -> Handler FilePath
abCover ab = (`replaceFileName` "cover.jpg") <$> abFullPath ab
