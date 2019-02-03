{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import           Database               (AbAuthor, AbSeries, AbTitle,
                                         AppDbConnection, RunSQL (..), runSQL)
import qualified Database.SQLite.Simple as Sql
import           Import.NoFoundation
import           Library                (MonadApplication)
import           Text.Hamlet            (hamletFile)

data App = App
    { appSettings     :: AppSettings
    , appStatic       :: EmbeddedStatic
    , appLogger       :: Logger
    , appDbConnection :: AppDbConnection
    }

mkYesodData "App" $(parseRoutesFile "routes")
mkMessage "App" "messages" "en"

instance Yesod App where
    makeLogger site = return (appLogger site)

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent $(widgetFileReload def "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RunSQL (HandlerFor App) where
    dbConnection = appDbConnection <$> getYesod

runSQLGetOr404 :: (Sql.Connection -> IO (Maybe a)) -> Handler a
runSQLGetOr404 = (>>= maybe notFound return) . runSQL

instance ReadSettings (HandlerFor App) where
    asksSettings = appSettings <$> getYesod

instance RunSQL (ReaderT App (ResourceT IO)) where
    dbConnection = asks appDbConnection

instance ReadSettings (ReaderT App (ResourceT IO)) where
    asksSettings = asks appSettings

instance {-# OVERLAPPING #-} MonadLogger (ReaderT App (ResourceT IO)) where
    monadLoggerLog loc src level msg = do
        logger <- asks appLogger
        liftIO $ loggerPutStr logger (defaultLogStr loc src level (toLogStr msg))

runApplicationIO :: App -> (forall m. MonadApplication m => m a) -> IO a
runApplicationIO app val = runResourceT (runReaderT val app)
