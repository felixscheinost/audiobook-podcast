{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MonadConstraints where

import           ClassyPrelude.Yesod
import qualified Database.SQLite.Simple as SQL
import           Settings               (AppSettings)

class Monad m => ReadSettings m where
    asksSettings :: m AppSettings

class Monad m => RunSQL m where
    runSQL :: (SQL.Connection -> IO a) -> m a
