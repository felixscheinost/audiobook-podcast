{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings (
    AppSettings(..),
    getAppSettings
) where

import           ClassyPrelude.Yesod
import           Data.Aeson          (Result (..), fromJSON, withObject, (.!=),
                                      (.:?))
import           Options.Applicative
import           Text.Read           (readMaybe)
import           Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)
import           System.Directory (doesPathExist)
import           Control.Monad.Trans.Maybe

data AppSettings = AppSettings
    { appPort                 :: Int
    , appCalibreLibraryFolder :: String
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> AppSettings
        <$> o .:? "port" .!= 8090
        <*> o .:  "calibre-library" 

getAppSettings :: IO AppSettings
getAppSettings = 
    --exists <- liftIO $ doesPathExist "settings.yaml"
    --unless exists (putStrLn "Couldn't load config file: 'settings.yaml' doesn't exist")
    --guard exists
    -- liftIO $ loadYamlSettings ["settings.yaml"] [] ignoreEnv
    loadYamlSettings ["settings.yaml"] [] ignoreEnv