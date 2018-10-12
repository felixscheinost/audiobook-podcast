{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> AppSettings
        <$> o .:? "port" .!= 8090
        <*> o .:  "calibre-library"

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings ["settings.yaml"] [] ignoreEnv
