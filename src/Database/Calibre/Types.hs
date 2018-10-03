{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.Calibre.Types where

import Database.Beam
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Backend.SQL
import Database.SQLite.Simple (SQLData(..))
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.Internal (Field(..))
import Control.Applicative ((<|>), empty)
import Data.Text (Text, pack, toLower)
    
data AudioFormat =
    MP3
    deriving (Show, Read, Eq, Enum)

data AudiobookFormat
    = SingleFile AudioFormat
    | ZIP
    deriving (Show, Read, Eq) 

possibleAudiobookFormats :: [AudiobookFormat]
possibleAudiobookFormats = ZIP : map SingleFile (enumFrom $ toEnum 0)

toCalibreFormatStr :: AudiobookFormat -> Text
toCalibreFormatStr (SingleFile af) = pack $ show af
toCalibreFormatStr f = pack $ show f

toFileExtension :: AudiobookFormat -> Text
toFileExtension = toLower . toCalibreFormatStr

instance FromField AudiobookFormat where
    fromField f@(Field (SQLText t) _) = 
        foldr (<|>) empty $ flip map possibleAudiobookFormats $ \format ->
            if toCalibreFormatStr format == t then
                Ok format
            else
                returnError ConversionFailed f "unrecognized book format"
    fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance FromBackendRow Sqlite AudiobookFormat

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be AudiobookFormat where
    sqlValueSyntax = sqlValueSyntax . toCalibreFormatStr

instance (IsSql92ExpressionSyntax be) => HasSqlEqualityCheck be AudiobookFormat