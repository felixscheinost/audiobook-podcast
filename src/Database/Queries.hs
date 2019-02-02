{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Queries (
    getAudiobookByAuthorTitle,
    getAudiobookById,
    listBooks,
    listBooksQuery,
    listBooksQuery2,
    deleteAllAudiobooks,
    BookOrSeries
) where

import           Control.Monad                   (forM_)
import           Data.Maybe                      (fromMaybe)
import           Data.NonNull                    (NonNull (..))
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

data BookOrSeries = Book Int | Series (NonNull [Int])

getAudiobookByAuthorTitle :: AbAuthor -> AbTitle -> Connection -> IO (Maybe Audiobook)
getAudiobookByAuthorTitle _author _title conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ (abTitle b ==. val_ _title)
    guard_ (abAuthor b ==. val_ _author)
    return b


getAudiobookById :: Int -> Connection -> IO (Maybe Audiobook)
getAudiobookById _id conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ (abId b ==. val_ _id)
    return b

listBooks :: Connection -> IO [Audiobook]
listBooks = listBooksQuery Nothing

listBooksQuery :: Maybe Text -> Connection -> IO [Audiobook]
listBooksQuery mQuery conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- orderBy_
        (\b -> (asc_ (abAuthor b), asc_ (abSeries b), asc_ (abTitle b)))
        (all_ (dbAudiobooks db))
    forM_ (T.words (fromMaybe "" mQuery)) $ \word ->
        guard_ $ (abTitle b `like_` val_ (AbTitle $ "%" <> word <> "%"))
                ||. (abAuthor b `like_` val_ (AbAuthor $ "%" <> word <> "%"))
                ||. (fromMaybe_ (val_ $ AbSeries "") (abSeries b) `like_` val_ (AbSeries $ "%" <> word <> "%"))
    return b

listBooksQuery2 :: Maybe Text -> Connection -> IO [(AbAuthor, Maybe AbSeries, Maybe AbTitle)]
listBooksQuery2 mQuery conn = runBeamSqlite conn $ runSelectReturningList $ select $ nub_ $ do
    b <- orderBy_
        (\b -> (asc_ (abAuthor b), asc_ (abSeries b), asc_ (abTitle b)))
        (all_ (dbAudiobooks db))
    forM_ (T.words (fromMaybe "" mQuery)) $ \word ->
        guard_ $ (abTitle b `like_` val_ (AbTitle $ "%" <> word <> "%"))
                ||. (abAuthor b `like_` val_ (AbAuthor $ "%" <> word <> "%"))
                ||. (fromMaybe_ (val_ $ AbSeries "") (abSeries b) `like_` val_ (AbSeries $ "%" <> word <> "%"))
    return
        ( abAuthor b
        , abSeries b
        , if_ [isNothing_ (abSeries b) `then_` just_ (abTitle b)] (else_ nothing_)
        )

deleteAll :: forall be db delete table
            . IsSql92DeleteSyntax delete
           => DatabaseEntity be db (TableEntity table)
            -- ^ Table to delete from
           -> SqlDelete delete table
deleteAll (DatabaseEntity (DatabaseTable tblNm _)) = SqlDelete (deleteStmt tblNm alias Nothing)
    where
        supportsAlias = deleteSupportsAlias (Proxy @delete)
        tgtName = "delete_target"
        alias = if supportsAlias then Just tgtName else Nothing

deleteAllAudiobooks :: Connection -> IO ()
deleteAllAudiobooks conn = runBeamSqlite conn $ runDelete $ deleteAll (dbAudiobooks db)
