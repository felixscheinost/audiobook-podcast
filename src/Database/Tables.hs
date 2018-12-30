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

module Database.Tables where

import           Database.Beam
import           Import.NoFoundation

-------------------------------------
-- BOOK
-------------------------------------

data AudiobookT f = Audiobook
    { abId          :: Columnar f Int
    , abPath        :: Columnar f Text
    , abTitle       :: Columnar f Text
    , abAuthor      :: Columnar f Text
    , abSeries      :: Columnar f (Maybe Text)
    , abSeriesIndex :: Columnar f (Maybe Int)
    } deriving Generic

type Audiobook = AudiobookT Identity
type AudiobookId = PrimaryKey AudiobookT Identity

deriving instance Show Audiobook
deriving instance Eq Audiobook
deriving instance Show AudiobookId

instance Beamable AudiobookT
instance Table AudiobookT where
    data PrimaryKey AudiobookT f = AudiobookId (Columnar f Int) deriving Generic
    primaryKey = AudiobookId . abId
instance Beamable (PrimaryKey AudiobookT)

-------------------------------------
-- DATABASE
------------------------------------

newtype DB f = DB
    { dbAudiobooks       :: f (TableEntity AudiobookT)
    } deriving Generic

instance Database be DB

db :: DatabaseSettings be DB
db = defaultDbSettings `withDbModification`
    dbModification {
        dbAudiobooks = modifyTable (const "audiobooks") tableModification
    }
