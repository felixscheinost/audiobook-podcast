{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.Calibre.BookFormat(
    CalibreBookFormat(..),
    allCalibreBookFormats,
    bookFormatFileExtension
) where

import           Data.Foldable                    (asum)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite             (Sqlite)
import           Database.SQLite.Simple           (SQLData (..))
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal  (Field (..))
import           Database.SQLite.Simple.Ok
import           Types                            (AudioFormat (..),
                                                   audioFileExtension,
                                                   supportedAudioFormats)

data CalibreBookFormat
    = ZIP
    | Audio AudioFormat
    deriving (Show, Eq)

allCalibreBookFormats :: [CalibreBookFormat]
allCalibreBookFormats = ZIP : (Audio <$> supportedAudioFormats)

bookFormatFileExtension :: CalibreBookFormat -> Text
bookFormatFileExtension ZIP            = "zip"
bookFormatFileExtension (Audio format) = T.pack $ audioFileExtension format

toDatabaseValue :: CalibreBookFormat -> Text
toDatabaseValue = T.toUpper . bookFormatFileExtension

instance FromField CalibreBookFormat where
    fromField f@(Field (SQLText t) _) =
        asum $ flip map allCalibreBookFormats $ \format ->
            if toDatabaseValue format == t then
                Ok format
            else
                returnError ConversionFailed f "unrecognized book format"
    fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance FromBackendRow Sqlite CalibreBookFormat

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CalibreBookFormat where
    sqlValueSyntax = sqlValueSyntax . toDatabaseValue

instance (IsSql92ExpressionSyntax be) => HasSqlEqualityCheck be CalibreBookFormat
