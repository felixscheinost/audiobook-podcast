{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import           Control.Concurrent.MVar    (putMVar, takeMVar)
import           Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R
import qualified Database                   as DB
import           Types
import           Views
import           Web.Scotty.Trans           (get)

runDb :: ReaderT AppStateAndConnection IO a -> MyActionM a
runDb db = do
    state <- lift R.ask
    conn <- lift $ lift $ takeMVar (stateDbConn state)
    a <- lift $ lift $ runReaderT db (state, conn)
    lift $ lift $ putMVar (stateDbConn state) conn
    return a
  
controllers :: MyScottyM ()
controllers =
    get "/" $ runDb DB.getBooks >>= homeView
