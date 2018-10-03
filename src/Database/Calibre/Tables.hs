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
import           Database.Beam.Sqlite
import           Database.Calibre.Types
import           Import

data CalibreBookT f = CalibreBook
    { bookId    :: Columnar f Int
    , bookPath  :: Columnar f Text
    , bookTitle :: Columnar f Text
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

data CalibreDb f = CalibreDb
    { cbBooks   :: f (TableEntity CalibreBookT)
    , cbData   :: f (TableEntity CalibreBookDataT)
    } deriving Generic

instance Database be CalibreDb

calibreDb :: DatabaseSettings be CalibreDb
calibreDb = defaultDbSettings
            `withDbModification` dbModification {
                cbBooks = modifyTable (const "books") tableModification,
                cbData = modifyTable (const "data") $ CalibreBookData "id" "format" "name" (CalibreBookId "book")
            }                
