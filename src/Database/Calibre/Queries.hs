{-# LANGUAGE OverloadedStrings #-}

module Database.Calibre.Queries where

import Database.Calibre.Tables
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple (Connection)

getAllAudiobooks :: Connection -> IO [(Book, Data)]
getAllAudiobooks conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- all_ (cbBooks calibreDb)
    d <- oneToMany_ (cbData calibreDb) dataBook b
    guard_ (dataFormat d ==. "ZIP")
    return (b, d)