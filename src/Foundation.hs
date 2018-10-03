{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import           Control.Concurrent.MVar    (MVar)
import           Control.Monad.Trans.Reader (runReader)
import qualified Database.SQLite.Simple     as Sql
import           Import.NoFoundation
import           System.FilePath            ((</>))
import           Text.Hamlet                (hamletFile)
import           Yesod.Core.Types           (Logger)

data App = App
    { appSettings     :: AppSettings
    , appStatic       :: Static
    , appLogger       :: Logger
    , appDbConnection :: MVar Sql.Connection
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/default-layout.hamlet")

runSQL :: (Sql.Connection -> IO a) -> Handler a
runSQL f = do
    app <- getYesod
    liftIO $ do
        conn <- takeMVar (appDbConnection app)
        res <- f conn
        putMVar (appDbConnection app) conn
        return res