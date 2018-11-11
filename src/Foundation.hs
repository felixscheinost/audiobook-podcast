{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import qualified Database.SQLite.Simple       as Sql
import           Import.NoFoundation
import           Text.Hamlet                  (hamletFile)
import           Yesod.Core.Types             (Logger)

data App = App
    { appSettings         :: AppSettings
    , appStatic           :: EmbeddedStatic
    , appLogger           :: Logger
    , appDbConnection     :: MVar Sql.Connection
    , appBookConvertQueue :: TChan Int
    }

mkYesodData "App" $(parseRoutesFile "routes")
mkMessage "App" "messages" "en"

instance Yesod App where
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        currentRoute <- getCurrentRoute
        let currentIsSeries = case currentRoute of
                Just SeriesViewR           -> True
                Just (SingleSeriesViewR _) -> True
                _                          -> False
        let currentIs route = maybe False (route ==) currentRoute
        pc <- widgetToPageContent $(widgetFileReload def "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RunSQL (HandlerFor App) where
    runSQL f = do
        mConn <- appDbConnection <$> getYesod
        liftIO $ do
            conn <- takeMVar mConn
            res <- f conn
            putMVar mConn conn
            return res

instance ReadSettings (HandlerFor App) where
    asksSettings = appSettings <$> getYesod
