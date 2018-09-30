{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings (
    AppSettings(..)
) where

import           ClassyPrelude.Yesod
import           Data.Aeson          (Result (..), fromJSON, withObject, (.!=),
                                      (.:?))
import           Options.Applicative
import           Text.Read           (readMaybe)

data AppSettings = AppSettings
    { appPort                 :: Int
    , appCalibreLibraryFolder :: String
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> AppSettings
        <$> o .:? "port" .!= 8090
        <*> o .:  "calibre-library" 