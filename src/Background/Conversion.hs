{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Background.Conversion where

import Import
import Background.Foundation
import Control.Concurrent(threadDelay)
--import qualified Database.Calibre as DB
import Database.Calibre.BookFormat (CalibreBookFormat(ZIP, Audio))
import Audiobook (AudioFormat)
import qualified Audiobook as AB

audioFormatsToConvert :: [AudioFormat]
audioFormatsToConvert = filter (/= AB.Mp3) AB.supportedAudioFormats 

-- | CalibreBookFormat currently only contains formats that can be converted
calibreFormatsToConvert :: [CalibreBookFormat]
calibreFormatsToConvert = ZIP : (Audio <$> audioFormatsToConvert)

-- | Scans every 5 min for books in the DB that need conversion
scanBooksToConvertJob :: MonadBackground m => m ()
scanBooksToConvertJob = do
    logInfoN "Checking for books to convert"
    liftIO $ threadDelay (30 * 1000000)
    scanBooksToConvertJob