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

main :: IO ()
main = do
    opts <- runParser
    res <- calibreDbList opts
    case res of
        Left err -> putStrLn $ "error:" ++ err
        Right res -> putStrLn $ show $ filterAudiobooks res

filterAudiobooks :: [Book] -> [Book]
filterAudiobooks = filter hasZIP
    where
        hasZIP = any (isSuffixOf "zip") . formats

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
    <$> (optional $ strOption $ long "library-path" <> help "Path to the calibre library")
    <*> (option int $ long "port" <> help "The port to run the server on" <> value 8090)

{-|
Runs the command line parser
-}
runParser :: IO Opts
runParser = execParser opts
    where opts = info (optParser <**> helper)
                      (progDesc "Serve audiobooks from calibre library")
