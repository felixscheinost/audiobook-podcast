{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Calibre.Queries (
    getAudiobook, getAllAudiobooks, 
    getAllSeries, getAllAudiobooksInSeries
) where

import           Data.Text               (Text)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Calibre.Tables
import           Database.Calibre.Types
import           Database.SQLite.Simple  (Connection)

books = all_ (cbBooks calibreDb)
series = all_ (cbSeries calibreDb)
booksBookSort = orderBy_ (asc_ . bookSort) books
booksSeriesSort = orderBy_ (asc_ . bookSeriesIndex) books

seriesBookRelationship :: ManyToMany CalibreDb CalibreSeriesT CalibreBookT
seriesBookRelationship =
    manyToMany_ (cbBooksSeries calibreDb) bsSeries bsBook

joinAudiobookData b = do
    d <- oneToMany_ (cbData calibreDb) dataBook b
    guard_ (dataFormat d `in_` map val_ supportedCalibreBookFormats)
    return d

getAudiobook :: Int -> Connection -> IO (Maybe (CalibreBook, CalibreBookData))
getAudiobook _bookId conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- books
    d <- joinAudiobookData b
    guard_ (bookId b ==. val_ _bookId)
    return (b, d)

getAllAudiobooks :: Connection -> IO [(CalibreBook, CalibreBookData)]
getAllAudiobooks conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- booksBookSort
    d <- joinAudiobookData b
    return (b, d)

getAllSeries :: Connection -> IO [(CalibreSeries, Text)]
getAllSeries conn = runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select $ do
    (s, bIds) <- aggregate_ (\(s, b) -> (group_ s, sqliteGroupConcatOver distinctInGroup_ (bookId b))) $ do
        (s, b) <- seriesBookRelationship series books
        _ <- joinAudiobookData b
        return (s, b)
    return (s, fromMaybe_ (val_ "") bIds)

getAllAudiobooksInSeries :: Int -> Connection -> IO [(CalibreBook, CalibreBookData)]
getAllAudiobooksInSeries _seriesId conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    (s, b) <- seriesBookRelationship series booksSeriesSort
    d <- joinAudiobookData b
    guard_ (seriesId s ==. val_ _seriesId)
    return (b, d)
