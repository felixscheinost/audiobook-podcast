{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Calibre.Queries (
    getBook, 
    listBooks, listBooksMissingFormat,
    listSeries,
    listBooksInSeries,
    BookAndData
) where

import           Data.Text                   (Text)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Calibre.BookFormat (allCalibreBookFormats, CalibreBookFormat)
import           Database.Calibre.Tables
import           Database.SQLite.Simple      (Connection)

books = all_ (cbBooks calibreDb)
series = all_ (cbSeries calibreDb)
booksBookSort = orderBy_ (asc_ . bookSort) books
booksSeriesSort = orderBy_ (asc_ . bookSeriesIndex) books

seriesBookRelationship :: ManyToMany CalibreDb CalibreSeriesT CalibreBookT
seriesBookRelationship =
    manyToMany_ (cbBooksSeries calibreDb) bsSeries bsBook

joinAudiobookData = oneToMany_ (cbData calibreDb) dataBook

bookAndData = do
    b <- booksBookSort
    d <- joinAudiobookData b
    guard_ (dataFormat d `in_` (val_ <$> allCalibreBookFormats))
    return (b, d)

type BookAndData = (CalibreBook, CalibreBookData)

getBook :: Int -> Connection -> IO (Maybe BookAndData)
getBook _bookId conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    (b, d) <- bookAndData
    guard_ (bookId b ==. val_ _bookId)
    return (b, d)

listBooks :: Connection -> IO [BookAndData]
listBooks conn = runBeamSqlite conn $ runSelectReturningList $ select bookAndData

listBooksMissingFormat :: CalibreBookFormat -> Connection -> IO [CalibreBook]
listBooksMissingFormat missingFormat conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    (b, sumTarget) <- aggregate_ (\(b, d) -> 
                ( group_ b
                , sum_ (if_ [(dataFormat d ==. val_ missingFormat) `then_` val_ (1 :: Int)] (else_ (val_ 0)))
                )) $ do
        b <- books
        d <- joinAudiobookData b
        guard_ (dataFormat d `in_` (val_ <$> allCalibreBookFormats))
        return (b, d)
    guard_ (fromMaybe_ (val_ 0) sumTarget ==. val_ 0)
    return b

-- Returns each series with a comma-separated list of IDs
-- TODO: Return only one book per series?
listSeries :: Connection -> IO [(CalibreSeries, Text)]
listSeries conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    (s, bIds) <- aggregate_ (\(s, b) -> (group_ s, sqliteGroupConcatOver distinctInGroup_ (bookId b))) $ do
        (s, b) <- seriesBookRelationship series books
        d <- joinAudiobookData b
        guard_ (dataFormat d `in_` (val_ <$> allCalibreBookFormats))
        return (s, b)
    return (s, fromMaybe_ (val_ "") bIds)

listBooksInSeries :: Int -> Connection -> IO [BookAndData]
listBooksInSeries _seriesId  conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- booksSeriesSort
    (s, b) <- seriesBookRelationship series (return b)
    d <- joinAudiobookData b
    guard_ (dataFormat d `in_` (val_ <$> allCalibreBookFormats))
    guard_ (seriesId s ==. val_ _seriesId)
    return (b, d)