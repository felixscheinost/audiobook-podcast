{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Calibre.Tables where

import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Calibre.Types
import           Import

data BookT f = Book
    { _bookId    :: Columnar f Integer
    , _bookPath  :: Columnar f Text
    , _bookTitle :: Columnar f Text
    } deriving Generic

type Book = BookT Identity
type BookId = PrimaryKey BookT Identity

instance Beamable BookT
instance Beamable (PrimaryKey BookT)
deriving instance Show Book
deriving instance Show BookId

instance Table BookT where
    data PrimaryKey BookT f = BookId (Columnar f Integer) deriving Generic
    primaryKey = BookId . _bookId

data AuthorT f = Author
    { _authorId   :: Columnar f Integer
    , _authorName :: Columnar f Text
    } deriving Generic

type Author = AuthorT Identity
type AuthorId = PrimaryKey AuthorT Identity

instance Beamable AuthorT
instance Beamable (PrimaryKey AuthorT)
deriving instance Show Author
deriving instance Show AuthorId

instance Table AuthorT where
    data PrimaryKey AuthorT f = AuthorId (Columnar f Integer) deriving Generic
    primaryKey = AuthorId . _authorId

data DataT f = Data
    { _dataId     :: Columnar f Integer
    , _dataFormat :: Columnar f AudiobookFormat
    , _dataName   :: Columnar f Text
    , _dataBook   :: PrimaryKey BookT f
    } deriving Generic

type Data = DataT Identity
type DataId = PrimaryKey DataT Identity

instance Beamable DataT
instance Beamable (PrimaryKey DataT)
deriving instance Show Data
deriving instance Show DataId

instance Table DataT where
    data PrimaryKey DataT f = DataId (Columnar f Integer) deriving Generic
    primaryKey = DataId . _dataId