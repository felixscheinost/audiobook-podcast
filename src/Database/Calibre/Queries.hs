{-# LANGUAGE OverloadedStrings #-}

module Database.Calibre.Queries where

import Database.Calibre.Tables
import Database.Calibre.Types
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection)
import Yesod.Core

joinBookAndData = do
    b <- all_ (cbBooks calibreDb)
    d <- oneToMany_ (cbData calibreDb) dataBook b
    guard_ (dataFormat d `in_` map val_ supportedCalibreBookFormats)
    return (b, d)

getAudiobook :: Int -> Connection -> IO (Maybe (CalibreBook, CalibreBookData))
getAudiobook _bookId conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    (b, d) <- joinBookAndData
    guard_ (bookId b ==. val_ _bookId)
    return (b, d)

getAllAudiobooks :: Connection -> IO [(CalibreBook, CalibreBookData)]
getAllAudiobooks conn = runBeamSqlite conn $ runSelectReturningList $ select $ 
    orderBy_ (asc_ . bookTitle . fst) joinBookAndData