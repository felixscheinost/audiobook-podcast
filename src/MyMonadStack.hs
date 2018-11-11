{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MyMonadStack where

import           ClassyPrelude.Yesod
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader)
import qualified Control.Monad.Reader   as Reader
import qualified Database.SQLite.Simple as SQL
import           Settings               (AppSettings)

class Monad m => ReadSettings m where
    asksSettings :: m AppSettings

class Monad m => RunSQL m where
    runSQL :: (SQL.Connection -> IO a) -> m a

--type MonadQuery m r = (MonadIO m, MonadReader r m, HasSettings r, HasDB r)
--type MonadSettingsReader m r = (MonadReader r m, HasSettings r)
