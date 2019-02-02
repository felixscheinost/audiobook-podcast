{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Tables where

import           Data.Text                        (Text)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Sqlite             (Sqlite)
import           Database.Beam.Sqlite.Syntax      (SqliteSyntax)
import           Database.SQLite.Simple.FromField
import           Import.NoFoundation
import           Text.Blaze                       (ToMarkup)

-------------------------------------
-- BOOK
-------------------------------------

newtype AbTitle = AbTitle Text
    deriving (Show, Eq, Read, PathPiece, ToMarkup)

newtype AbAuthor = AbAuthor Text
    deriving (Show, Eq, Read, PathPiece, ToMarkup)

newtype AbSeries = AbSeries Text
    deriving (Show, Eq, Read, PathPiece, ToMarkup)

data AudiobookT f = Audiobook
    { abId          :: Columnar f Int
    , abPath        :: Columnar f Text
    , abTitle       :: Columnar f AbTitle
    , abAuthor      :: Columnar f AbAuthor
    , abSeries      :: Columnar f (Maybe AbSeries)
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


------------------------------------------------
-- Boilerplate for newtype Text wrappers
------------------------------------------------

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be AbTitle where
    sqlValueSyntax (AbTitle t) = sqlValueSyntax t
instance FromField AbTitle where
    fromField f = AbTitle <$> fromField f
instance FromBackendRow Sqlite AbTitle
instance IsSqlExpressionSyntaxStringType be AbTitle
instance IsSql92ExpressionSyntax be => HasSqlEqualityCheck be AbTitle

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be AbAuthor where
    sqlValueSyntax (AbAuthor t) = sqlValueSyntax t
instance FromField AbAuthor where
    fromField f = AbAuthor <$> fromField f
instance FromBackendRow Sqlite AbAuthor
instance IsSqlExpressionSyntaxStringType be AbAuthor
instance IsSql92ExpressionSyntax be => HasSqlEqualityCheck be AbAuthor

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be AbSeries where
    sqlValueSyntax (AbSeries t) = sqlValueSyntax t
instance FromField AbSeries where
    fromField f = AbSeries <$> fromField f
instance FromBackendRow Sqlite AbSeries
instance IsSqlExpressionSyntaxStringType be AbSeries
instance IsSql92ExpressionSyntax be => HasSqlEqualityCheck be AbSeries
