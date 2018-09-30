{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Controllers where

import           Control.Concurrent.MVar       (putMVar, takeMVar)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Reader    (ReaderT (..))
import qualified Control.Monad.Trans.Reader    as R
import qualified Database                      as DB
import           System.FilePath               ((</>))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (hamletFile)
import           Types
import           Views
import           Web.Scotty.Trans              (get, html)

runDb :: ReaderT AppStateAndConnection IO a -> MyActionM a
runDb db = do
    state <- lift R.ask
    conn <- lift $ lift $ takeMVar (stateDbConn state)
    a <- lift $ lift $ runReaderT db (state, conn)
    lift $ lift $ putMVar (stateDbConn state) conn
    return a

controllers :: MyScottyM ()
controllers =
    get "/" $ do
        books <- runDb DB.getBooks 
        html $ renderHtml $ $(hamletFile "templates/index.hamlet") (\_ _ -> undefined)
