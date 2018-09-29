{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Concurrent.MVar    (MVar)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text.Lazy             (Text)
import qualified Database.SQLite.Simple     as Sql
import           System.FilePath            ((</>))
import           Web.Scotty.Trans           (ActionT, ScottyT)

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

data AudiobookFormat
  = M4B
  | ZIP
  | MP3
  deriving (Show, Read, Eq, Enum)

data Audiobook = Audiobook
  { abId     :: Integer
  , abTitle  :: String
  , abFormat :: AudiobookFormat
  , abPath   :: FilePath
  } deriving (Show)

type MyScottyM = ScottyT Text (ReaderT AppState IO)

type MyActionM = ActionT Text (ReaderT AppState IO)
