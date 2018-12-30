{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Queries (
    getAudiobook,
    listBooks,
) where

import           Data.Proxy
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92 (IsSql92DeleteSyntax,
                                                  deleteStmt,
                                                  deleteSupportsAlias)
import           Database.Beam.Schema.Tables     (DatabaseEntity (..), DatabaseEntityDescriptor (DatabaseTable))
import           Database.Beam.Sqlite
import           Database.SQLite.Simple          (Connection)
import           Database.Tables

getAudiobook :: Int -> Connection -> IO (Maybe Audiobook)
getAudiobook _bookId conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ (abId b ==. val_ _bookId)
    return b

listBooks :: Connection -> IO [Audiobook]
listBooks conn = runBeamSqlite conn $ runSelectReturningList $ select $ all_ (dbAudiobooks db)

deleteAll :: forall be db delete table
            . IsSql92DeleteSyntax delete
           => DatabaseEntity be db (TableEntity table)
            -- ^ Table to delete from
           -> SqlDelete delete table
deleteAll (DatabaseEntity (DatabaseTable tblNm tblSettings)) = SqlDelete (deleteStmt tblNm alias Nothing)
    where
        supportsAlias = deleteSupportsAlias (Proxy @delete)
        tgtName = "delete_target"
        alias = if supportsAlias then Just tgtName else Nothing

deleteAllAudiobooks :: Connection -> IO ()
deleteAllAudiobooks conn = runBeamSqlite conn $ runDelete $ deleteAll (dbAudiobooks db)
