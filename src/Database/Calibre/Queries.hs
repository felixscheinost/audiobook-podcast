{-# LANGUAGE OverloadedStrings #-}

module Database.Calibre.Queries where

import Database.Calibre.Tables
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection)
import Yesod.Core

joinBookAndData = do
    b <- all_ (cbBooks calibreDb)
    d <- oneToMany_ (cbData calibreDb) dataBook b
    guard_ (dataFormat d ==. "ZIP")
    return (b, d)

getAudiobook :: Int -> Connection -> IO (Maybe (Book, Data))
getAudiobook _bookId conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    (b, d) <- joinBookAndData
    guard_ (bookId b ==. val_ _bookId)
    return (b, d)

getAllAudiobooks :: Connection -> IO [(Book, Data)]
getAllAudiobooks conn = runBeamSqlite conn $ runSelectReturningList $ select joinBookAndData 