{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.Queries (
    getAudiobookByAuthorTitle,
    getAudiobooksByAuthorSeries,
    getAudiobookById,
    listBooksQuery,
    deleteAllAudiobooks,
) where

import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam
import Database.Beam.Backend.SQL (BeamSqlBackend)
import           Database.Beam.Sqlite
import           Database.SQLite.Simple          (Connection)
import           Database.Tables
import           Import.NoFoundation             hiding (Proxy)

getAudiobookByAuthorTitle :: AbAuthor -> AbTitle -> Connection -> IO (Maybe Audiobook)
getAudiobookByAuthorTitle _author _title conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ (abTitle b ==. val_ _title)
    guard_ (abAuthor b ==. val_ _author)
    return b

getAudiobooksByAuthorSeries :: AbAuthor -> AbSeries -> Connection -> IO [Audiobook]
getAudiobooksByAuthorSeries _author _series conn = runBeamSqlite conn $ runSelectReturningList $ select $ do
    b <- orderBy_ (asc_ . abSeriesIndex) (all_ (dbAudiobooks db))
    guard_ (abSeries b ==. just_ (val_ _series))
    guard_ (abAuthor b ==. val_ _author)
    return b

getAudiobookById :: Int -> Connection -> IO (Maybe Audiobook)
getAudiobookById _id conn = runBeamSqlite conn $ runSelectReturningOne $ select $ do
    b <- all_ (dbAudiobooks db)
    guard_ (abId b ==. val_ _id)
    return b

justAbTitle :: BeamSqlBackend be => AudiobookT (QExpr be s) -> QExpr be s (Maybe AbTitle)
justAbTitle b = just_ (abTitle b)

listBooksQuery :: Maybe Text -> Connection -> IO [(AbAuthor, Maybe AbSeries, Maybe AbTitle)]
listBooksQuery mQuery conn = runBeamSqlite conn $ runSelectReturningList $ select $ nub_ $ do
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
        , if_ [isNothing_ (abSeries b) `then_` (justAbTitle b)] (else_ nothing_)
        )

deleteAllAudiobooks :: Connection -> IO ()
deleteAllAudiobooks conn = runBeamSqlite conn $ runDelete $ 
    delete (dbAudiobooks db) (const $ val_ True)
