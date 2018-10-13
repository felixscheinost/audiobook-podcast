{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Settings (
    AppSettings(..),
    getAppSettings
) where

import           ClassyPrelude.Yesod
import           Data.Aeson            (withObject, (.!=), (.:?))
import           Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)

data AppSettings = AppSettings
    { appPort                 :: Int
    , appCalibreLibraryFolder :: String
    , appDevelopment          :: Bool
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
        return AppSettings {..}

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings ["settings.yaml"] [] ignoreEnv
