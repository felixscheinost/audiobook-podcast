{-# LANGUAGE OverloadedStrings #-}

module Database
  ( getBooks
  ) where

import           Data.Char              (toLower)
import           Data.Text              (Text)
import           Database.SQLite.Simple
import           System.FilePath        ((<.>), (</>))
import           Types

data AudiobookPathQueryResult = AudiobookPathQueryResult
  { apqId     :: Integer
  , apqFormat :: String
  , apqPath   :: String
  , apqName   :: String
  }

instance FromRow AudiobookPathQueryResult where
  fromRow = AudiobookPathQueryResult <$> field <*> field <*> field <*> field

data AudiobookMetadataQueryResult = AudiobookMetadataQueryResult
  { amqId    :: Integer
  , amqTitle :: String
  , amqName  :: String
  }

instance FromRow AudiobookMetadataQueryResult where
  fromRow = AudiobookMetadataQueryResult <$> field <*> field <*> field

merge ::
     Opts
  -> AudiobookPathQueryResult
  -> AudiobookMetadataQueryResult
  -> Audiobook
merge opts l r =
  Audiobook
    (apqId l)
    (amqTitle r)
    (read $ apqFormat l)
    (libraryFolder opts </> apqPath l </> apqName l <.>
     map toLower (apqFormat l))

toAudiobook ::
     Opts
  -> [Audiobook]
  -> [AudiobookPathQueryResult]
  -> [AudiobookMetadataQueryResult]
  -> [Audiobook]
toAudiobook _ res [] _ = reverse res
toAudiobook _ res _ [] = reverse res
toAudiobook opts res (l:ls) (r:rs)
  | apqId l == amqId r = toAudiobook opts (merge opts l r : res) ls rs
  | apqId l < amqId r = toAudiobook opts res ls (r : rs)
  | apqId l > amqId r = toAudiobook opts res (l : ls) rs

getBooks :: Opts -> Connection -> IO [Audiobook]
getBooks opts conn
    -- Filter relevant formats and join with data table to get path to book
 = do
  apqBooks <-
    query_
      conn
      "SELECT b.id, MAX(d.format), b.path, d.name \
                \ FROM books AS b \
                \ LEFT JOIN data AS d ON b.id=d.book \
                \ WHERE d.format in ('ZIP') \
                \ GROUP BY b.title \
                \ ORDER BY b.id" :: IO [AudiobookPathQueryResult]
    -- Fetch metadata for relevant books
  amqBooks <-
    query
      conn
      "SELECT b.id, b.title, a.name \
                \ FROM books AS b \
                \ LEFT JOIN books_authors_link AS l ON b.id=l.book \
                \ LEFT JOIN authors AS a ON a.id=l.author\
                \ WHERE b.id IN (?) \
                \ ORDER BY b.id"
      (map apqId apqBooks) :: IO [AudiobookMetadataQueryResult]
  return $ toAudiobook opts [] apqBooks amqBooks
