{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Database.Calibre.Tables where

import           Database.Beam
import           Database.Calibre.BookFormat
import           Import


-------------------------------------
-- BOOK
-------------------------------------

data CalibreBookT f = CalibreBook
    { bookId          :: Columnar f Int
    , bookPath        :: Columnar f Text
    , bookTitle       :: Columnar f Text
    , bookSort        :: Columnar f Text
    , bookSeriesIndex :: Columnar f Float
    } deriving Generic

type CalibreBook = CalibreBookT Identity
type CalibreBookId = PrimaryKey CalibreBookT Identity

deriving instance Show CalibreBook
deriving instance Eq CalibreBook
deriving instance Show CalibreBookId

instance Beamable CalibreBookT
instance Table CalibreBookT where
    data PrimaryKey CalibreBookT f = CalibreBookId (Columnar f Int) deriving Generic
    primaryKey = CalibreBookId . bookId
instance Beamable (PrimaryKey CalibreBookT)

-------------------------------------
-- DATA
------------------------------------

data CalibreBookDataT f = CalibreBookData
    { dataId     :: Columnar f Int
    , dataFormat :: Columnar f CalibreBookFormat
    , dataName   :: Columnar f Text
    , dataBook   :: PrimaryKey CalibreBookT f
    } deriving Generic

type CalibreBookData = CalibreBookDataT Identity
type CalibreBookDataId = PrimaryKey CalibreBookDataT Identity

instance Beamable CalibreBookDataT
instance Beamable (PrimaryKey CalibreBookDataT)
deriving instance Show CalibreBookData
deriving instance Show CalibreBookDataId

instance Table CalibreBookDataT where
    data PrimaryKey CalibreBookDataT f = CalibreBookDataId (Columnar f Int) deriving Generic
    primaryKey = CalibreBookDataId . dataId


data BookAndData = BookAndData
    { bdBook :: CalibreBook
    , bdData :: CalibreBookData
    }

-------------------------------------
-- SERIES
------------------------------------

data CalibreSeriesT f = CalibreSeries
    { seriesId   :: Columnar f Int
    , seriesName :: Columnar f Text
    } deriving Generic

type CalibreSeries = CalibreSeriesT Identity
type CalibreSeriesId = PrimaryKey CalibreSeriesT Identity

instance Beamable CalibreSeriesT
instance Beamable (PrimaryKey CalibreSeriesT)
deriving instance Show CalibreSeries
deriving instance Show CalibreSeriesId

instance Table CalibreSeriesT where
    data PrimaryKey CalibreSeriesT f = CalibreSeriesId (Columnar f Int) deriving Generic
    primaryKey = CalibreSeriesId . seriesId

-------------------------------------
-- BOOKS-SERIES-LINK
------------------------------------

data CalibreBookSeriesLinkT f = CalibreBookSeriesLink
    { bsId     :: Columnar f Int
    , bsSeries :: PrimaryKey CalibreSeriesT f
    , bsBook   :: PrimaryKey CalibreBookT f
    } deriving Generic

type CalibreBookSeriesLink = CalibreBookSeriesLinkT Identity
type CalibreBookSeriesLinkId = PrimaryKey CalibreBookSeriesLinkT Identity

instance Beamable CalibreBookSeriesLinkT
instance Beamable (PrimaryKey CalibreBookSeriesLinkT)
deriving instance Show CalibreBookSeriesLink
deriving instance Show CalibreBookSeriesLinkId

instance Table CalibreBookSeriesLinkT where
    data PrimaryKey CalibreBookSeriesLinkT f = CalibreBookSeriesLinkId (Columnar f Int) deriving Generic
    primaryKey = CalibreBookSeriesLinkId . bsId

-------------------------------------
-- DATABASE
------------------------------------

data CalibreDb f = CalibreDb
    { cbBooks       :: f (TableEntity CalibreBookT)
    , cbData        :: f (TableEntity CalibreBookDataT)
    , cbSeries      :: f (TableEntity CalibreSeriesT)
    , cbBooksSeries :: f (TableEntity CalibreBookSeriesLinkT)
    } deriving Generic

instance Database be CalibreDb

calibreDb :: DatabaseSettings be CalibreDb
calibreDb = defaultDbSettings `withDbModification`
    dbModification {
        cbBooks = modifyTable (const "books") tableModification,
        cbData = modifyTable (const "data") $
            CalibreBookData "id" "format" "name" (CalibreBookId "book"),
        cbSeries = modifyTable (const "series") tableModification,
        cbBooksSeries = modifyTable (const "books_series_link") $
            CalibreBookSeriesLink "id" (CalibreSeriesId "series") (CalibreBookId "book")
    }
