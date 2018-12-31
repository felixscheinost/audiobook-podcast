{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Queries (
    getAudiobook,
    listBooks,
    listBooksQuery,
    deleteAllAudiobooks
) where

import           Control.Monad                   (forM)
import           Data.Maybe                      (fromMaybe)
import           Data.Proxy
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92 (IsSql92DeleteSyntax,
                                                  deleteStmt,
                                                  deleteSupportsAlias)
import           Database.Beam.Schema.Tables     (DatabaseEntity (..), DatabaseEntityDescriptor (DatabaseTable))
import           Database.Beam.Sqlite
import           Database.SQLite.Simple          (Connection)
import           Database.Tables

getAudiobook :: Text -> Text -> Connection -> IO (Maybe Audiobook)
getAudiobook _author _title conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ (abTitle b ==. val_ _title)
    guard_ (abAuthor b ==. val_ _author)
    return b

listBooks :: Connection -> IO [Audiobook]
listBooks = listBooksQuery Nothing

listBooksQuery :: Maybe Text -> Connection -> IO [Audiobook]
listBooksQuery mQuery conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ $
        let
            word = fromMaybe "" mQuery
        -- foldl (&&.) $ forM (maybe [] T.words mQuery) $ \word ->
        in
            (abTitle b `like_` val_ ("%" <> word <> "%"))
                ||. (abAuthor b `like_` val_ ("%" <> word <> "%"))
                ||. (fromMaybe_ (val_ "") (abSeries b) `like_` val_ ("%" <> word <> "%"))
    return b

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
deleteAllAudiobooks conn = runBeamSqliteDebug print conn $ runDelete $ deleteAll (dbAudiobooks db)
