{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import           Control.Concurrent.MVar    (putMVar, takeMVar)
import           Control.Monad.Trans.Class  (lift)
import qualified Control.Monad.Trans.Reader as R
import qualified Database                   as DB
import           Types
import           Views
import           Web.Scotty.Trans           (get)

home :: MyScottyM ()
home =
  get "/" $ do
    state <- lift R.ask
    conn <- lift $ lift $ takeMVar (stateDbConn state)
    books <- lift $ lift $ DB.getBooks (stateOpts state) conn
    lift $ lift $ putMVar (stateDbConn state) conn
    homeView books
