{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Settings (
    AppSettings(..),
    getAppSettings,
    widgetFile,
    widgetFileReload,
    def,
    Mp3Bitrate,
    ffmpegBitrate, 
) where

import           ClassyPrelude.Yesod
import           Data.Aeson                 (withObject, (.!=), (.:?), withScientific)
import qualified Data.Scientific as S
import           Language.Haskell.TH.Syntax (Exp, Q)
import           Yesod.Default.Config2      (ignoreEnv, loadYamlSettings)
import           Yesod.Default.Util         (WidgetFileSettings,
                                             widgetFileNoReload,
                                             widgetFileReload)

newtype Mp3Bitrate = Mp3Bitrate Int

defaultBitrate :: Mp3Bitrate
defaultBitrate = Mp3Bitrate 128

ffmpegBitrate :: Mp3Bitrate -> String
ffmpegBitrate (Mp3Bitrate b) = show b ++ "k"

validBitrates :: [Int]
validBitrates = [8, 16, 24, 32, 40, 48, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320]

mp3Bitrate :: Int -> Maybe Mp3Bitrate
mp3Bitrate b = 
    if b `elem` validBitrates then Just (Mp3Bitrate b)
    else Nothing

instance FromJSON Mp3Bitrate where
    parseJSON = withScientific "number" $ maybe failure return <$> (S.toBoundedInteger >=> mp3Bitrate)
        where
            validBitratesStr = intercalate ", " $ map show validBitrates
            failure = fail $ "bitrate needs to be one of the values " ++ validBitratesStr

data AppSettings = AppSettings
    { appPort                 :: Int
    , appCalibreLibraryFolder :: String
    , appDevelopment          :: Bool
    , appMp3Bitrate           :: Mp3Bitrate
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appPort                 <- o .:? "port" .!= 8090
        appCalibreLibraryFolder <- o .:  "calibre-library"
        let defaultDevelopment =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        appDevelopment          <- o .:? "development" .!= defaultDevelopment
        appMp3Bitrate           <- o .:? "mp3-bitrate" .!= defaultBitrate
        return AppSettings {..}

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings ["settings.yaml"] [] ignoreEnv

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

widgetFile :: String -> Q Exp
widgetFile =
#ifdef DEVELOPMENT
    widgetFileReload def
#else
    widgetFileNoReload def
#endif
