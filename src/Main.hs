{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative                  (optional)
import           Control.Monad.Trans.Class            (lift)
import qualified Control.Monad.Trans.Reader           as R
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8           as C
import           Data.List                            (isSuffixOf)
import           Data.Semigroup                       ((<>))
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Options.Applicative
import           System.Exit                          (ExitCode (..))
import           System.Process
import           Text.Read                            (readMaybe)
import           Types
import           Views                                (homeView)
import           Web.Scotty.Trans

main :: IO ()
main = do
    opts <- runParser
    scottyT (port opts) (`R.runReaderT` opts) app

app :: MyScottyM ()
app = do
    middleware $ staticPolicy (noDots >-> addBase "assets")
    middleware logStdout
    home

home :: MyScottyM ()
home = get "/" $ do
    opts <- lift R.ask
    books <- lift $ lift $ getBooks opts
    homeView books

filterAudiobooks :: [Book] -> [Book]
filterAudiobooks = filter hasCorrectFormat
    where
        correctSuffix f = any (`isSuffixOf` f) ["zip", "mp3", "m4b"]
        hasCorrectFormat = any correctSuffix . bookFormats
{-|
Fetches books from the CalibreDB and returns only those that contain audiobooks
-}
getBooks :: Opts -> IO [Book]
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
        args = maybe basicArgs libraryArgs $ libraryPath opts

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
    <$> optional (strOption $ long "library-path" <> help "Path to the calibre library")
    <*> option int (long "port" <> help "The port to run the server on" <> value 8090)

{-|
Runs the command line parser
-}
runParser :: IO Opts
runParser = execParser opts
    where opts = info (optParser <**> helper)
                      (progDesc "Serve audiobooks from calibre library")
