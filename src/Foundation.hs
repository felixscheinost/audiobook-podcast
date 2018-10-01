{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Foundation where

import Text.Hamlet          (hamletFile)
import Yesod.Core.Types     (Logger)
import Import.NoFoundation
import Control.Concurrent.MVar (MVar)
import qualified Database.SQLite.Simple as Sql

data App = App
    { appSettings :: AppSettings
    , appStatic :: Static
    , appLogger      :: Logger
    , appDbConnection :: MVar Sql.Connection
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/default-layout.hamlet")
