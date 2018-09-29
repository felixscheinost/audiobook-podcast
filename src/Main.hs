{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.MVar              (newMVar, putMVar,
                                                       takeMVar)
import           Control.Monad                        (void)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import qualified Control.Monad.Trans.Reader           as R
import           Data.Semigroup                       ((<>))
import qualified Database                             as DB
import qualified Database.SQLite.Simple               as SQL
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Options.Applicative
import           System.Directory                     (doesPathExist)
import           Text.Read                            (readMaybe)
import           Types
import           Views                                (homeView)
import           Web.Scotty.Trans                     (get, middleware, scottyT)

main :: IO ()
main =
  (void . runMaybeT) $ do
    opts <- lift runParser >>= checkOptions
    state <- lift $ setupState opts
    lift $ scottyT (port opts) (`R.runReaderT` state) app

checkOptions :: Opts -> MaybeT IO Opts
checkOptions opts =
  MaybeT $ do
    let path = libraryPath opts
    exists <- doesPathExist path
    if not exists
      then do
        putStrLn $ "No such file '" ++ path ++ "'"
        return Nothing
      else return (Just opts)

setupState :: Opts -> IO AppState
setupState opts =
  AppState <$> return opts <*> (SQL.open (libraryPath opts) >>= newMVar)

app :: MyScottyM ()
app = do
  middleware $ staticPolicy (noDots >-> addBase "assets")
  middleware logStdout
  home

home :: MyScottyM ()
home =
  get "/" $ do
    state <- lift R.ask
    conn <- lift $ lift $ takeMVar (stateDbConn state)
    books <- lift $ lift $ DB.getBooks (stateOpts state) conn
    lift $ lift $ putMVar (stateDbConn state) conn
    homeView books

int :: ReadM Int
int = eitherReader parse
  where
    err s = Left $ "'" ++ s ++ "' is not a number"
    parse s = maybe (err s) Right (readMaybe s)

{-|
The options parser
-}
optParser :: Parser Opts
optParser =
  Opts <$>
  option int (long "port" <> help "The port to run the server on" <> value 8090) <*>
  argument str (metavar "CALIBRE_FOLDER" <> help "Path to the calibre library")

{-|
Runs the command line parser
-}
runParser :: IO Opts
runParser = execParser opts
  where
    opts =
      info
        (optParser <**> helper)
        (progDesc "Serve audiobooks from calibre library")
