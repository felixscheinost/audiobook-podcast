{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Database.Calibre (
    Audiobook(..),
    abCover,
    possibleAudiobookFormats
) where

import           Import
import           System.FilePath (replaceFileName, (</>))

libraryPath :: AppSettings -> FilePath
libraryPath o = appCalibreLibraryFolder o </> "metadata.db"

data AudioFormat = 
    MP3
    deriving (Show, Read, Eq, Enum)

data AudiobookFormat
    = SingleFile AudioFormat
    | ZIP
    deriving (Show, Read, Eq)

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