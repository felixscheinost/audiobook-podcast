{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Queries (
    getAudiobook,
    listBooks,
) where

import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple (Connection)
import           Database.Tables

getAudiobook :: Int -> Connection -> IO (Maybe Audiobook)
getAudiobook _bookId conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ (abId b ==. val_ _bookId)
    return b

listBooks :: Connection -> IO [Audiobook]
listBooks conn = runBeamSqlite conn $ runSelectReturningList $ select $ all_ (dbAudiobooks db)
