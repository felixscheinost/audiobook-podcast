{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.Calibre.Types where

import           Control.Applicative              (empty, (<|>))
import           Data.Foldable                    (asum)
import           Data.List                        (find)
import           Data.Text                        (Text, pack, toLower, unpack)
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite             (Sqlite)
import           Database.SQLite.Simple           (SQLData (..))
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal  (Field (..))
import           Database.SQLite.Simple.Ok

data CalibreBookFormat
    = ZIP
    | MP3 
    | M4A
    | M4B
    deriving (Show, Enum, Eq)

supportedCalibreBookFormats :: [CalibreBookFormat]
supportedCalibreBookFormats = enumFrom $ toEnum 0

toFileExtension :: CalibreBookFormat -> Text
toFileExtension = toLower . pack . show

instance FromField CalibreBookFormat where
    fromField f@(Field (SQLText t) _) =
        asum $ flip map supportedCalibreBookFormats $ \format ->
            if pack (show format) == t then
                Ok format
            else
                returnError ConversionFailed f "unrecognized book format"
    fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance FromBackendRow Sqlite CalibreBookFormat

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CalibreBookFormat where
    sqlValueSyntax = sqlValueSyntax . pack . show

instance (IsSql92ExpressionSyntax be) => HasSqlEqualityCheck be CalibreBookFormat
