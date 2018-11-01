{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Calibre.Queries (
    getBook, 
    listBooks, listMp3Books,
    listSeries, listSeriesWithMp3Books,
    listBooksInSeries, listMp3BooksInSeries
) where

import           Data.Text                   (Text)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Calibre.BookFormat (CalibreBookFormat(Audio))
import           Database.Calibre.Tables
import           Database.SQLite.Simple      (Connection)
import           Types                       (AudioFormat (Mp3))

books = all_ (cbBooks calibreDb)
series = all_ (cbSeries calibreDb)
booksBookSort = orderBy_ (asc_ . bookSort) books
booksSeriesSort = orderBy_ (asc_ . bookSeriesIndex) books

mapBookAndData :: Functor f => IO (f (CalibreBook, CalibreBookData)) -> IO (f BookAndData)
mapBookAndData = fmap (fmap $ uncurry BookAndData)

seriesBookRelationship :: ManyToMany CalibreDb CalibreSeriesT CalibreBookT
seriesBookRelationship =
    manyToMany_ (cbBooksSeries calibreDb) bsSeries bsBook

joinAudiobookData = oneToMany_ (cbData calibreDb) dataBook

getBook :: Int -> Connection -> IO (Maybe BookAndData)
getBook _bookId conn = mapBookAndData $ runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- books
    d <- joinAudiobookData b
    guard_ (bookId b ==. val_ _bookId)
    return (b, d)

listBooks :: [CalibreBookFormat] -> Connection -> IO [BookAndData]
listBooks formats conn = mapBookAndData $ runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- booksBookSort
    d <- joinAudiobookData b
    guard_ (dataFormat d `in_` (val_ <$> formats))
    return (b, d)


listMp3Books :: Connection -> IO [BookAndData]
listMp3Books = listBooks [Audio Mp3]

-- Returns each series with a comma-separated list of IDs
-- TODO: Return only one book per series?
listSeries :: [CalibreBookFormat] -> Connection -> IO [(CalibreSeries, Text)]
listSeries formats conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    (s, bIds) <- aggregate_ (\(s, b) -> (group_ s, sqliteGroupConcatOver distinctInGroup_ (bookId b))) $ do
        (s, b) <- seriesBookRelationship series books
        d <- joinAudiobookData b
        guard_ (dataFormat d `in_` (val_ <$> formats))
        return (s, b)
    return (s, fromMaybe_ (val_ "") bIds)

listSeriesWithMp3Books :: Connection -> IO [(CalibreSeries, Text)]
listSeriesWithMp3Books = listSeries [Audio Mp3]

listBooksInSeries :: [CalibreBookFormat] -> Int -> Connection -> IO [BookAndData]
listBooksInSeries formats _seriesId  conn = mapBookAndData $ runBeamSqlite conn $ runSelectReturningList $ select $ do
    (s, b) <- seriesBookRelationship series booksSeriesSort
    d <- joinAudiobookData b
    guard_ (dataFormat d `in_` (val_ <$> formats))
    guard_ (seriesId s ==. val_ _seriesId)
    return (b, d)

listMp3BooksInSeries :: Int -> Connection -> IO [BookAndData]
listMp3BooksInSeries = listBooksInSeries [Audio Mp3]