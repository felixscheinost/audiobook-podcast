{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foundation where

import           Control.Concurrent.MVar    (MVar)
import qualified Database.SQLite.Simple     as Sql
import           Import.NoFoundation
import           Text.Hamlet                (hamletFile)
import           Yesod.Core.Types           (Logger)

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

runSQL :: (Sql.Connection -> IO a) -> Handler a
runSQL f = do
    app <- getYesod
    liftIO $ do
        conn <- takeMVar (appDbConnection app)
        res <- f conn
        putMVar (appDbConnection app) conn
        return res