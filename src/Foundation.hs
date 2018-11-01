{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Foundation where

import           Control.Concurrent.MVar (MVar)
import qualified Database.SQLite.Simple  as Sql
import           Import.NoFoundation
import           Text.Hamlet             (hamletFile)
import           Yesod.Core.Types        (Logger)

data App = App
    { appSettings     :: AppSettings
    , appStatic       :: EmbeddedStatic
    , appLogger       :: Logger
    , appDbConnection :: MVar Sql.Connection
    }

mkYesodData "App" $(parseRoutesFile "routes")
mkMessage "App" "messages" "en"

data MenuItem = MenuItem
    { menuItemMsg     :: AppMessage
    , menuItemRoute   :: Route App
    , menuItemIcon    :: Text
    , menuItemMatcher :: Maybe (Route App -> Bool)
    }


instance Yesod App where
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        let isSeriesRoute route = case route of
                SeriesViewR           -> True
                (SingleSeriesViewR _) -> True
                _                     -> False
        let menuItems =
                [ MenuItem
                    { menuItemMsg = MsgBookView
                    , menuItemRoute = BookViewR
                    , menuItemIcon = "book-open"
                    , menuItemMatcher = Nothing
                    } 
                , MenuItem
                    { menuItemMsg = MsgSeriesView
                    , menuItemRoute = SeriesViewR
                    , menuItemIcon = "list"
                    , menuItemMatcher = Just isSeriesRoute
                    }
                , MenuItem
                    { menuItemMsg = MsgAuthorView
                    , menuItemRoute = AuthorViewR
                    , menuItemIcon = "user"
                    , menuItemMatcher = Nothing
                    }
                , MenuItem
                    { menuItemMsg = MsgConversionsView
                    , menuItemRoute = AuthorViewR
                    , menuItemIcon = "cogs"
                    , menuItemMatcher = Nothing
                    }
                , MenuItem
                    { menuItemMsg = MsgSettingsView
                    , menuItemRoute = AuthorViewR
                    , menuItemIcon = "wrench"
                    , menuItemMatcher = Nothing
                    }
                ]
        let menuClass mi = [("class" :: Text, "fas fa-" ++ menuItemIcon mi)]
        currentRoute <- getCurrentRoute
        let matcher mi = fromMaybe (== menuItemRoute mi) (menuItemMatcher mi)
        let isActive mi = maybe False (matcher mi) currentRoute
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
