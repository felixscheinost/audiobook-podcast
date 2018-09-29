{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Concurrent.MVar        (MVar)
import           Control.Monad.Trans.Reader     (ReaderT)
import qualified Data.Text                      as T
import           Data.Text.Lazy                 (Text)
import           Database.SQLite.Simple         (SQLData(SQLText))
import qualified Database.SQLite.Simple         as Sql
import           Database.SQLite.Simple.ToField
import           System.FilePath                ((</>))
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

data AudiobookFormat
  = M4B
  | ZIP
  | MP3
  deriving (Show, Read, Eq, Enum)

instance ToField AudiobookFormat where
    toField = SQLText . T.pack . show 

data Audiobook = Audiobook
  { abId      :: Integer
  , abTitle   :: String
  , abAuthors :: [String]
  , abFormat  :: AudiobookFormat
  , abPath    :: FilePath
  } deriving (Show)

type MyScottyM = ScottyT Text (ReaderT AppState IO)

type MyActionM = ActionT Text (ReaderT AppState IO)
