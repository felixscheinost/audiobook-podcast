{-# LANGUAGE OverloadedStrings #-}

module Database
  ( getBooks
  ) where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.Char                  (toLower)
import           Data.List                  (intersperse)
import           Data.Text                  (Text, pack)
import           Database.SQLite.Simple
import           System.FilePath            ((<.>), (</>))
import           Types
import Debug.Trace 

data AudiobookPathQueryResult = AudiobookPathQueryResult
  { apqId     :: Integer
  , apqFormat :: AudiobookFormat
  , apqPath   :: String
  , apqName   :: String
  }

instance FromRow AudiobookPathQueryResult where
  fromRow = AudiobookPathQueryResult <$> field <*> field <*> field <*> field

data AudiobookMetadataQueryResult = AudiobookMetadataQueryResult
  { amqId     :: Integer
  , amqTitle  :: String
  , amqAuthor :: String
  } deriving (Show)

instance FromRow AudiobookMetadataQueryResult where
  fromRow = AudiobookMetadataQueryResult <$> field <*> field <*> field

------------------------------------------------------------------------------
-- | Create Audiobook from result of both queries and the program options
audiobookFrom :: Opts -> AudiobookPathQueryResult -> AudiobookMetadataQueryResult-> Audiobook
audiobookFrom opts l r =  Audiobook
    (apqId l)
    (amqTitle r)
    [amqAuthor r]
    (apqFormat l)
    (libraryFolder opts </> apqPath l </> apqName l <.> fileExtension (apqFormat l))

------------------------------------------------------------------------------
-- | Add audiobook to result of already created audiobooks.
-- | If the previous created audiobook has the same as the new one, just add the current author to the new one.
-- | The reason for that is that we JOIN on authors so a book with multiple authors will result
-- | in `merge` being called for the same book with different authors.
merge :: Opts -> AudiobookPathQueryResult -> AudiobookMetadataQueryResult -> [Audiobook] -> [Audiobook]
merge opts l r [] = [audiobookFrom opts l r]
merge opts l r (a:as)
    | abId a == apqId l =
        -- add author to book at the front of the list
        a { abAuthors = amqAuthor r : abAuthors a } : as
    | otherwise =
        audiobookFrom opts l r : a : as

------------------------------------------------------------------------------
-- | Create a list of audiobooks from
-- |  - a list of distinct audiobooks, containing the format and path to the audiobook
-- |  - a list of audiobooks JOINED with their author -> multiple entries for multiple authors
-- |  Both lists are sorted by ID ASC.
-- |   => Go through both lists at once and match books
toAudiobook :: Opts -> [Audiobook] -> [AudiobookPathQueryResult] -> [AudiobookMetadataQueryResult] -> [Audiobook]
toAudiobook _ res [] _ = reverse res
toAudiobook _ res _ [] = reverse res
toAudiobook opts res (l:ls) (r:rs)
  | apqId l == amqId r = toAudiobook opts (merge opts l r res) (l:ls) rs
  | apqId l < amqId r = toAudiobook opts res ls (r : rs)
  | apqId l > amqId r = toAudiobook opts res (l : ls) rs

queryParameters :: Int -> String
queryParameters i = intersperse ',' $ replicate i '?'

getBooks :: ReaderT AppStateAndConnection IO [Audiobook]
getBooks = do
    (state, conn) <- ask
    -- Filter relevant formats and join with data table to get path to book
    apqBooks <- lift (query conn
        (Query $ pack ("SELECT b.id, MAX(d.format), b.path, d.name \
        \ FROM books AS b \
        \ LEFT JOIN data AS d ON b.id=d.book \
        \ WHERE d.format IN (" ++ queryParameters (length possibleAudiobookFormats) ++ ") \
        \ GROUP BY b.title \
        \ ORDER BY b.id"))
        possibleAudiobookFormats)
    -- Fetch metadata for relevant books
    amqBooks <- lift (query conn
        (Query $ pack ("SELECT b.id, b.title, a.name \
        \ FROM books AS b \
        \ LEFT JOIN books_authors_link AS l ON b.id=l.book \
        \ LEFT JOIN authors AS a ON a.id=l.author\
        \ WHERE b.id IN (" ++queryParameters (length apqBooks) ++ ") \
        \ ORDER BY b.id"))
        (map apqId apqBooks))
    return $ toAudiobook (stateOpts state) [] apqBooks amqBooks
