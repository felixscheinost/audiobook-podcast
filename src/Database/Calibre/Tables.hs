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

data BookT f = Book
    { bookId    :: Columnar f Int
    , bookPath  :: Columnar f Text
    , bookTitle :: Columnar f Text
    } deriving Generic

type Book = BookT Identity
type BookId = PrimaryKey BookT Identity

deriving instance Show Book
deriving instance Eq Book
deriving instance Show BookId

instance Beamable BookT
instance Table BookT where
    data PrimaryKey BookT f = BookId (Columnar f Int) deriving Generic
    primaryKey = BookId . bookId
instance Beamable (PrimaryKey BookT)

data AuthorT f = Author
    { authorId   :: Columnar f Int
    , authorName :: Columnar f Text
    } deriving Generic

type Author = AuthorT Identity
type AuthorId = PrimaryKey AuthorT Identity

instance Beamable AuthorT
instance Beamable (PrimaryKey AuthorT)
deriving instance Show Author
deriving instance Show AuthorId

instance Table AuthorT where
    data PrimaryKey AuthorT f = AuthorId (Columnar f Int) deriving Generic
    primaryKey = AuthorId . authorId

data DataT f = Data
    { dataId     :: Columnar f Int
    --, dataFormat :: Columnar f AudiobookFormat
    , dataFormat :: Columnar f Text
    , dataName   :: Columnar f Text
    , dataBook   :: PrimaryKey BookT f
    } deriving Generic

type Data = DataT Identity
type DataId = PrimaryKey DataT Identity

instance Beamable DataT
instance Beamable (PrimaryKey DataT)
deriving instance Show Data
deriving instance Show DataId

instance Table DataT where
    data PrimaryKey DataT f = DataId (Columnar f Int) deriving Generic
    primaryKey = DataId . dataId

data CalibreDb f = CalibreDb
    { cbBooks   :: f (TableEntity BookT)
    , cbAuthors :: f (TableEntity AuthorT)
    , cbData   :: f (TableEntity DataT)
    } deriving Generic

instance Database be CalibreDb

calibreDb :: DatabaseSettings be CalibreDb
calibreDb = defaultDbSettings
            `withDbModification` dbModification {
                cbBooks = modifyTable (const "books") tableModification,
                cbAuthors = modifyTable (const "authors") tableModification,
                cbData = modifyTable (const "data") $ Data "id" "format" "name" (BookId "book")
            }                
