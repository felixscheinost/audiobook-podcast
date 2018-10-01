{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre (
    libraryPath,
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

--abCover :: Audiobook -> FilePath
--abCover ab = replaceFileName (abPath ab) "cover.jpg"