{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
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

getAllSeries :: Connection -> IO [(CalibreSeries, Maybe Text)]
getAllSeries conn = runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select $
    aggregate_ (\(series, book) -> (group_ series, sqliteGroupConcatOver distinctInGroup_ (bookId book))) $ do
        s <- all_ (cbSeries calibreDb)
        bs <- join_ (cbBooksSeries calibreDb) (\link -> bsSeries link ==. pk s)
        b <- join_ (cbBooks calibreDb) (\book -> bsBook bs ==. pk book)
        d <- join_ (cbData calibreDb) (\_data -> dataBook _data ==. pk b)
        guard_ (dataFormat d `in_` map val_ supportedCalibreBookFormats)
        return (s, b)
