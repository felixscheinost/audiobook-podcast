{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Settings (
    AppSettings(..),
    getAppSettings,
    widgetFile,
    widgetFileReload,
    def
) where

import           ClassyPrelude.Yesod
import           Data.Aeson                 (withObject, (.!=), (.:?))
import           Language.Haskell.TH.Syntax (Exp, Q)
import           Yesod.Default.Config2      (ignoreEnv, loadYamlSettings)
import           Yesod.Default.Util         (WidgetFileSettings,
                                             widgetFileNoReload,
                                             widgetFileReload)

data AppSettings = AppSettings
    { appPort                 :: Int
    , appCalibreLibraryFolder :: String
    , appDevelopment          :: Bool
    , appMp3Quality       :: Int
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appPort                 <- o .:? "port" .!= 8090
        appCalibreLibraryFolder <- o .:  "calibre-library"
        quality                 <- o .:?  "mp3-quality" .!= 7
        appMp3Quality <- 
            if 0 <= quality && quality <= 9 then return quality
            else fail "mp3-quality neds to be between 0-9"
        let defaultDevelopment =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        appDevelopment          <- o .:? "development" .!= defaultDevelopment
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
