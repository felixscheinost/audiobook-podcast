{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (optional)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Options.Applicative
import Data.Aeson
import System.Process
import System.Exit (ExitCode(..))
import Debug.Trace (traceShow)
import Data.List (isSuffixOf)
import qualified Data.ByteString.Lazy.Char8 as C
import Text.Read(readMaybe)
import Web.Spock
import Web.Spock.Config

data Opts = Opts
    { libraryPath :: Maybe String
    , port :: Int
    }

data Book = Book
    { authors :: String
    , title :: String
    , formats :: [String]
    } deriving (Show)

instance FromJSON Book where
  parseJSON = withObject "Book" $ \v -> Book
    <$> v .: "authors"
    <*> v .: "title"
    <*> v .: "formats"

type Server state = SpockM () state () ()
newtype State = WithBooks [Book]

main :: IO ()
main = do
    opts <- runParser
    books <- getBooks
    spockCfg <- defaultSpockCfg (WithBooks books) PCNoDatabase ()
    runSpock (port opts) (spock spockCfg app)
                
app :: Server State
app = get root (text "Hello World!")

filterAudiobooks :: [Book] -> [Book]
filterAudiobooks = filter hasZIP
    where
        hasZIP = any (isSuffixOf "zip") . formats

getBooks :: IO [Book]
getBooks = do
    -- TODO: Don't run parser everytime (state?)
    opts <- runParser
    res <- calibreDbList opts
    case res of
        Left err -> do putStrLn ("error getting books:" ++ err)
                       return []

        Right res -> return (filterAudiobooks res)

{-|
Run `calibredb list`.
Parses the resulting JSON into a list of books or returns an error string.
-}
calibreDbList :: Opts -> IO (Either String [Book])
calibreDbList opts = do
    (code, stdout, stderr) <- readProcessWithExitCode "calibredb" args ""
    return $ case code of
        ExitSuccess ->
            case decode $ C.pack stdout of
                Just books -> Right books
                Nothing -> Left "couldn't parse result"

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
