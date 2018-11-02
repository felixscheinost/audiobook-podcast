{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Background.Foundation where

import           Control.Monad.Logger
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Time                  (getZonedTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import qualified Database.SQLite.Simple     as Sql
import           Import
import           Yesod.Core.Types           (Logger, loggerPutStr)

-- Similar to `Application`
data Background = Background
    { backgroundSettings     :: AppSettings
    , backgroundDbConnection :: MVar Sql.Connection
    , backgroundLogger       :: Logger
    }

-- Similar to `Handler`
type MonadBackground m =
    ( MonadIO m
    , MonadLogger m
    , MonadReader Background m
    --, MonadUnliftIO m
    )

useYesodLogger :: Logger -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
useYesodLogger logger loc logSrc logLvl logStr = do
    time <- getZonedTime
    let timeStr = formatTime defaultTimeLocale "[%d/%b/%Y:%T %z] " time
    loggerPutStr logger $ toLogStr timeStr <> defaultLogStr loc logSrc logLvl logStr

runBackground :: MonadIO m => Background -> ReaderT Background (LoggingT m) a -> m a
runBackground b r = runLoggingT (runReaderT r b) (useYesodLogger $ backgroundLogger b)
