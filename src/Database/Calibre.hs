{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    Audiobook(..),
    abCover,
    possibleAudiobookFormats
) where

import           Database.Beam
import           Database.Beam.Sqlite
import           Import
import           System.FilePath      (replaceFileName, (</>))
import Database.Calibre.Types

libraryPath :: AppSettings -> FilePath
libraryPath o = appCalibreLibraryFolder o </> "metadata.db"

possibleAudiobookFormats :: [AudiobookFormat]
possibleAudiobookFormats = ZIP : map SingleFile (enumFrom $ toEnum 0)

data Audiobook = Audiobook
    { abId      :: Integer
    , abTitle   :: String
    , abAuthors :: [String]
    , abFormat  :: AudiobookFormat
    , abPath    :: FilePath
    } deriving (Show)

abCover :: Audiobook -> FilePath
abCover ab = replaceFileName (abPath ab) "cover.jpg"