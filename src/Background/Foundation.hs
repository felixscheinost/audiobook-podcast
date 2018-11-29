{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Background.Foundation where

import           Control.Monad.Trans.Reader (ReaderT)
import           Conversion.Audiobook       (Conversion)
import qualified Database.SQLite.Simple     as Sql
import           Foundation                 (App)
import           Import

data Background = Background
    { backgroundSettings             :: AppSettings
    , backgroundLogger               :: Logger
    , backgroundDbConnection         :: MVar (Maybe Sql.Connection)
    , backgroundBookIdToConvertQueue :: TChan Int
    , backgroundConversionQueue      :: TChan Conversion
    , backgroundConversions          :: TVar [Conversion]
    }

type BackgroundAction = ReaderT Background IO

instance ReadSettings BackgroundAction where
    asksSettings = asks backgroundSettings

instance RunSQL BackgroundAction where
    dbConnection = asks backgroundDbConnection

runBackground :: App -> BackgroundAction b -> IO b
runBackground App{..} background = do
    let b = Background
            { backgroundSettings = appSettings
            , backgroundLogger = appLogger
            , backgroundDbConnection = appDbConnection
            , backgroundBookIdToConvertQueue = appBookIdToConvertQueue
            , backgroundConversionQueue = appConversionQueue
            , backgroundConversions = appConversions
            }
    runReaderT background b
