{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import           Conversion.Audiobook         (Conversion)
import           Database.Calibre             (RunSQL (..))
import           Database.Calibre             (libraryPath)
import qualified Database.SQLite.Simple       as Sql
import           Database.SQLite3             (SQLError)
import           Import.NoFoundation
import           Text.Hamlet                  (hamletFile)

data App = App
    { appSettings             :: AppSettings
    , appStatic               :: EmbeddedStatic
    , appLogger               :: Logger
    , appDbConnection         :: MVar (Maybe (Sql.Connection))
    -- Frontend queues ID to be converted
    , appBookIdToConvertQueue :: TChan Int
    -- Backend queues conversation
    , appConversionQueue      :: TChan Conversion
    , appConversions          :: TVar [Conversion]
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
    dbConnection = appDbConnection <$> getYesod

instance ReadSettings (HandlerFor App) where
    asksSettings = appSettings <$> getYesod
