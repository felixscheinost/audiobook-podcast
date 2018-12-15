{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}

module Database.Calibre.Audiobook (
    listAudiobooks,
    listAudiobooksNeedConversion,
    listAudiobooksInSeries,
    getAudiobook,
    listSeries,
    calibreBookToAudiobook,
    Audiobook(..),
) where

import           Data.Function           (on)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (mapMaybe)
import           Database.Calibre        (RunSQL (..), runSQL)
import qualified Database.Calibre        as Database
import           Database.Calibre.Format (CalibreBookFormat (Audio))
import           Database.Calibre.Tables (CalibreBook, CalibreBookData,
                                          CalibreSeries)
import           Import.NoFoundation
import qualified Settings

data Audiobook = Audiobook
    { abId    :: Int
    , abTitle :: Text
    , abCover :: FilePath
    }

playableFormats :: ReadSettings m => m [CalibreBookFormat]
playableFormats = do
    settings <- asksSettings
    return $ Audio <$> Settings.appDirectPlayFormats settings

listAudiobooks :: (ReadSettings m, RunSQL m) => m [Audiobook]
listAudiobooks = playableFormats
    >>= (runSQL . Database.listBooks)
    >>= mapM calibreBookToAudiobook

listAudiobooksNeedConversion :: (ReadSettings m, RunSQL m) => m [Audiobook]
listAudiobooksNeedConversion = playableFormats
    >>= (runSQL . Database.listBooksMissingFormats)
    >>= mapM calibreBookToAudiobook

listAudiobooksInSeries :: (ReadSettings m, RunSQL m) => Int -> m [Audiobook]
listAudiobooksInSeries _seriesId = playableFormats
        >>= (runSQL . Database.listBooksMissingFormats)
        >>= mapM calibreBookToAudiobook

listSeries :: (ReadSettings m, RunSQL m) => m [(CalibreSeries, Text)]
listSeries = playableFormats >>= (runSQL . Database.listSeries)

getAudiobook :: (MonadHandler m, ReadSettings m, RunSQL m) => Int -> m Audiobook
getAudiobook _id = runSQL (Database.getBook _id) >>= maybe notFound calibreBookToAudiobook

listBooksWithFormatsForIds :: (ReadSettings m, RunSQL m) => [Int] -> m [(Audiobook, [CalibreBookData])]
listBooksWithFormatsForIds ids = do
    formats <- playableFormats
    bookDatas <- runSQL $ Database.listBookDataForIds ids formats
    let toMapEntry (book, bookData) = (book, [bookData])
    let toAudiobook (book, bookDatas) = (, bookDatas) <$> calibreBookToAudiobook book
    mapM toAudiobook $ Map.toList $ Map.fromListWith (++) (fmap toMapEntry bookDatas)

calibreBookToAudiobook :: ReadSettings m => CalibreBook -> m Audiobook
calibreBookToAudiobook book = do
    let abId = Database.bookId book
    let abTitle = Database.bookTitle book
    abCover <- Database.bookCover book
    return Audiobook{..}
