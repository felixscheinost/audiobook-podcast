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

import           Control.Concurrent.MVar (MVar)
import           Database                (RunSQL (..))
import qualified Database.SQLite.Simple  as Sql
import           Import.NoFoundation
import           Text.Hamlet             (hamletFile)

data App = App
    { appSettings     :: AppSettings
    , appStatic       :: EmbeddedStatic
    , appLogger       :: Logger
    , appDbConnection :: MVar Sql.Connection
    }

mkYesodData "App" $(parseRoutesFile "routes")
mkMessage "App" "messages" "en"

instance Yesod App where
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent $(widgetFileReload def "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RunSQL (HandlerFor App) where
    dbConnection = appDbConnection <$> getYesod

instance ReadSettings (HandlerFor App) where
    asksSettings = appSettings <$> getYesod
