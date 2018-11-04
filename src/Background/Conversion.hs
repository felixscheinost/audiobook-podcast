{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Background.Conversion where

import qualified Audiobook                   as AB
import           Background.Foundation
import           Control.Concurrent          (threadDelay)
import qualified Database.Calibre            as DB
import           Database.Calibre.BookFormat (CalibreBookFormat (Audio, ZIP))
import           Import                      hiding (runSQL)

-- audioFormatToConvertTo :: AudioFormat
-- audioFormatToConvertTo = AB.Mp3

-- audioFormatsToConvert :: [AudioFormat]
-- audioFormatsToConvert = filter (/= audioFormatToConvertTo) AB.supportedAudioFormats

-- -- | CalibreBookFormat currently only contains formats that can be converted
-- calibreFormatsToConvert :: [CalibreBookFormat]
-- calibreFormatsToConvert = ZIP : (Audio <$> audioFormatsToConvert)


-- | Scans every 5 min for books in the DB that need conversion
scanBooksToConvertJob :: MonadBackground m => m ()
scanBooksToConvertJob = do
    logInfoN "Checking for books to convert"
    -- books <- runSQL $ DB.listBooksToConvert calibreFormatsToConvert (Audio audioFormatToConvertTo)
    -- mapM_ print books
    liftIO $ threadDelay (5 * 60 * 1000000)
    scanBooksToConvertJob
