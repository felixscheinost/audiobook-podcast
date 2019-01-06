{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Settings (
    AppSettings(..),
    ReadSettings(..),
    getAppSettings,
    widgetFile,
    widgetFileReload,
    def,
    runSettingsReader
) where

import           ClassyPrelude.Yesod
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (withObject, (.!=), (.:?))
import           Language.Haskell.TH.Syntax (Exp, Q)
import           Yesod.Default.Config2      (ignoreEnv, loadYamlSettings)
import           Yesod.Default.Util         (WidgetFileSettings,
                                             widgetFileNoReload,
                                             widgetFileReload)

data AppSettings = AppSettings
    { appPort            :: Int
    , appLibraryFolder   :: FilePath
    , appDevelopment     :: Bool
    , appAudioExtensions :: [Text]
    }

class Monad m => ReadSettings m where
    asksSettings :: m AppSettings

runSettingsReader :: ReadSettings m => ReaderT AppSettings n a -> m (n a)
runSettingsReader r = runReaderT r <$> asksSettings

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appPort          <- o .:? "port" .!= 8090
        let defaultAudioExtensions = ["mp3"]
        appAudioExtensions <- o .:? "audio-files" .!= defaultAudioExtensions
        appLibraryFolder <- o .:  "library"
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
