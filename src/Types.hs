{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Concurrent.MVar        (MVar)
import           Control.Monad.Trans.Reader     (ReaderT)
import Data.Char (toLower)
import qualified Data.Text                      as T
import           Data.Text.Lazy                 (Text)
import           Database.SQLite.Simple         (SQLData(SQLText))
import qualified Database.SQLite.Simple         as Sql
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.Internal (Field(..))
import           System.FilePath                ((</>), replaceFileName)
import           Web.Scotty.Trans               (ActionT, ScottyT)

data Opts = Opts
  { port          :: Int
  , libraryFolder :: String
  }

libraryPath :: Opts -> FilePath
libraryPath o = libraryFolder o </> "metadata.db"

data AppState = AppState
  { stateOpts   :: Opts
  , stateDbConn :: MVar Sql.Connection
  }

type AppStateAndConnection = (AppState, Sql.Connection)

data AudioFormat = 
    MP3
    deriving (Show, Read, Eq, Enum)

data AudiobookFormat
  = SingleFile AudioFormat
  | ZIP
  deriving (Show, Read, Eq)

possibleAudiobookFormats :: [AudiobookFormat]
possibleAudiobookFormats = ZIP : map SingleFile (enumFrom $ toEnum 0)

fileExtension :: AudiobookFormat -> String
fileExtension (SingleFile f) = map toLower $ show f
fileExtension f = map toLower $ show f

instance ToField AudiobookFormat where
    toField (SingleFile format) = SQLText $ T.pack $ show format
    toField format = SQLText $ T.pack $ show format

instance FromField AudiobookFormat where
    fromField f@(Field (SQLText t) _) 
        | format == "mp3" = Ok $ SingleFile MP3
        | format == "zip" = Ok ZIP
        | otherwise = returnError ConversionFailed f ("unknown format '" ++ format ++ "'")
        where
            format = T.unpack $ T.toLower t
    fromField f                     = returnError ConversionFailed f "expecting SQLText column type"

data Audiobook = Audiobook
  { abId      :: Integer
  , abTitle   :: String
  , abAuthors :: [String]
  , abFormat  :: AudiobookFormat
  , abPath    :: FilePath
  } deriving (Show)

abCover :: Audiobook -> FilePath
abCover ab = replaceFileName (abPath ab) "cover.jpg"

type MyScottyM = ScottyT Text (ReaderT AppState IO)

type MyActionM = ActionT Text (ReaderT AppState IO)
