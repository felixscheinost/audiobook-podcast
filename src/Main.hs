{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import           Control.Monad.Trans.Class            (lift)
import qualified Control.Monad.Trans.Reader           as R
import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.Semigroup                       ((<>))
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Options.Applicative
import           Text.Read                            (readMaybe)
import           Types
import           Views                                (homeView)
import           Web.Scotty.Trans (scottyT, get, middleware)
import System.Directory (doesPathExist)
import qualified Database.SQLite.Simple as SQL
import qualified Database as DB

main :: IO ()
main = (void . runMaybeT) $ do
    opts <- lift runParser >>= checkOptions
    state <- lift $ setupState opts
    lift $ scottyT (port opts) (`R.runReaderT` state) app

checkOptions :: Opts -> MaybeT IO Opts
checkOptions opts = MaybeT $ do
    let path = libraryPath opts
    exists <- doesPathExist path
    if not exists  then do
        putStrLn $ "No such file '" ++ path ++ "'"
        return Nothing
    else
        return (Just opts)

setupState :: Opts -> IO AppState
setupState opts = AppState
    <$> return opts
    <*> (SQL.open (libraryPath opts) >>= newMVar)

app :: MyScottyM ()
app = do
    middleware $ staticPolicy (noDots >-> addBase "assets")
    middleware logStdout
    home

home :: MyScottyM ()
home = get "/" $ do
    state <- lift R.ask
    conn <- lift $ lift $ takeMVar (stateDbConn state)
    books <- lift $ lift $ DB.getBooks (stateOpts state) conn
    lift $ lift $ putMVar (stateDbConn state) conn
    homeView books

--withConnection :: (SQL.Connection -> MyScottyM ()) -> MyScottyM ()
--withConnection f = ScottyT $ do
{-|
Fetches books from the CalibreDB and returns only those that contain audiobooks
-}
{-getBooks :: Opts -> IO [Book]
getBooks opts = do
    putStrLn "Reading Calibre DB"
    res <- calibreDbList opts
    case res of
        Left err -> do putStrLn ("error getting books:" ++ err)
                       return []
        Right books -> return (filterAudiobooks books)

{-|
Run `calibredb list`.
Parses the resulting JSON into a list of books or returns an error string.
-}
calibreDbList :: Opts -> IO (Either String [Book])
calibreDbList opts = do
    (res, stdout, stderr) <- readProcessWithExitCode "calibredb" args ""
    return $ case res of
        ExitSuccess ->
            case decode $ C.pack stdout of
                Just books -> Right books
                Nothing    -> Left "couldn't parse result"

        ExitFailure code -> Left $ "calibredb returned code " ++ show code ++ ":" ++ stderr
    where
        basicArgs = ["list", "--fields", "title,authors,formats", "--for-machine"]
        libraryArgs lib = basicArgs ++ ["--with-library", lib]
        args = maybe basicArgs libraryArgs $ libraryPath opts-}

int :: ReadM Int
int = eitherReader parse
    where
        err s = Left $ "'" ++ s ++ "' is not a number"
        parse s = maybe (err s) Right (readMaybe s)

{-|
The options parser
-}
optParser :: Parser Opts
optParser = Opts
    <$> option int (long "port" <> help "The port to run the server on" <> value 8090)
    <*> argument str (metavar "CALIBRE_FOLDER" <> help "Path to the calibre library")

{-|
Runs the command line parser
-}
runParser :: IO Opts
runParser = execParser opts
    where opts = info (optParser <**> helper)
                      (progDesc "Serve audiobooks from calibre library")
