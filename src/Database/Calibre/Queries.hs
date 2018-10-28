{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Calibre.Queries (
    getAudiobook, getAllAudiobooks,
    getAllSeries, getAllAudiobooksInSeries
) where

import           Data.Text                   (Text)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Calibre.BookFormat (supportedCalibreBookFormats)
import           Database.Calibre.Tables
import           Database.SQLite.Simple      (Connection)

books = all_ (cbBooks calibreDb)
series = all_ (cbSeries calibreDb)
booksBookSort = orderBy_ (asc_ . bookSort) books
booksSeriesSort = orderBy_ (asc_ . bookSeriesIndex) books

mapBookAndData :: Functor f => IO (f (CalibreBook, CalibreBookData)) -> IO (f BookAndData)
mapBookAndData = fmap (fmap $ uncurry BookAndData)

seriesBookRelationship :: ManyToMany CalibreDb CalibreSeriesT CalibreBookT
seriesBookRelationship =
    manyToMany_ (cbBooksSeries calibreDb) bsSeries bsBook

joinAudiobookData b = do
    d <- oneToMany_ (cbData calibreDb) dataBook b
    guard_ (dataFormat d `in_` map val_ supportedCalibreBookFormats)
    return d

getAudiobook :: Int -> Connection -> IO (Maybe BookAndData)
getAudiobook _bookId conn = mapBookAndData $ runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- books
    d <- joinAudiobookData b
    guard_ (bookId b ==. val_ _bookId)
    return (b, d)

getAllAudiobooks :: Connection -> IO [BookAndData]
getAllAudiobooks conn =mapBookAndData $ runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- booksBookSort
    d <- joinAudiobookData b
    return (b, d)

-- Returns each series with a comma-separated list of IDs
-- TODO: Return only one book per series?
getAllSeries :: Connection -> IO [(CalibreSeries, Text)]
getAllSeries conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    (s, bIds) <- aggregate_ (\(s, b) -> (group_ s, sqliteGroupConcatOver distinctInGroup_ (bookId b))) $ do
        (s, b) <- seriesBookRelationship series books
        _ <- joinAudiobookData b
        return (s, b)
    return (s, fromMaybe_ (val_ "") bIds)

getAllAudiobooksInSeries :: Int -> Connection -> IO [BookAndData]
getAllAudiobooksInSeries _seriesId conn = mapBookAndData $ runBeamSqlite conn $ runSelectReturningList $ select $ do
    (s, b) <- seriesBookRelationship series booksSeriesSort
    d <- joinAudiobookData b
    guard_ (seriesId s ==. val_ _seriesId)
    return (b, d)
