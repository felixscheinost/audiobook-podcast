{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Calibre.Queries where

import           Data.Text               (Text)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Calibre.Tables
import           Database.Calibre.Types
import           Database.SQLite.Simple  (Connection)

seriesBookRelationship :: ManyToMany CalibreDb CalibreSeriesT CalibreBookT
seriesBookRelationship =
    manyToMany_ (cbBooksSeries calibreDb) bsSeries bsBook

joinAudiobookData b = do
    d <- oneToMany_ (cbData calibreDb) dataBook b
    guard_ (dataFormat d `in_` map val_ supportedCalibreBookFormats)
    return d

getAudiobook :: Int -> Connection -> IO (Maybe (CalibreBook, CalibreBookData))
getAudiobook _bookId conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (cbBooks calibreDb)
    d <- joinAudiobookData b
    guard_ (bookId b ==. val_ _bookId)
    return (b, d)

getAllAudiobooks :: Connection -> IO [(CalibreBook, CalibreBookData)]
getAllAudiobooks conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- orderBy_ (asc_ . bookSort) (all_ (cbBooks calibreDb))
    d <- joinAudiobookData b
    return (b, d)

getAllSeries :: Connection -> IO [(CalibreSeries, Text)]
getAllSeries conn = runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select $ do
    (s, bIds) <- aggregate_ (\(series, book) -> (group_ series, sqliteGroupConcatOver distinctInGroup_ (bookId book))) $ do
        (s, b) <- seriesBookRelationship (all_ (cbSeries calibreDb)) (all_ (cbBooks calibreDb))
        d <- join_ (cbData calibreDb) (\_data -> dataBook _data ==. pk b)
        guard_ (dataFormat d `in_` map val_ supportedCalibreBookFormats)
        return (s, b)
    return (s, fromMaybe_ (val_ "") bIds)

