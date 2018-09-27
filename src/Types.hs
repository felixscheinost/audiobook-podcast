{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson
import           Data.Text.Lazy             (Text)
import           Web.Scotty.Trans           (ActionT, ScottyT)

data Opts = Opts
  { libraryPath :: Maybe String
  , port        :: Int
  }

data Book = Book
  { bookAuthors :: String
  , bookTitle   :: String
  , bookFormats :: [String]
  } deriving (Show)

instance FromJSON Book where
  parseJSON =
    withObject "Book" $ \v ->
      Book <$> v .: "authors" <*> v .: "title" <*> v .: "formats"

type MyScottyM = ScottyT Text (ReaderT Opts IO)

type MyActionM = ActionT Text (ReaderT Opts IO)
