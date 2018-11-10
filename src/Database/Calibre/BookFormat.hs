{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric         #-}

module Database.Calibre.BookFormat(
    CalibreBookFormat(..),
    AudioFormat(..),
    allCalibreBookFormats,
    bookFormatFileExtension,
    filePathAudioFormat,
    ffmpegFormatStr
) where

import qualified Data.Char                        as C
import qualified Data.List                        as L
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Sqlite             (Sqlite)
import           Database.SQLite.Simple           (SQLData (..))
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal  (Field (..))
import           Database.SQLite.Simple.Ok
import qualified System.FilePath                  as FP
import Data.Aeson (FromJSON)

data AudioFormat
    = Mp3
    | M4a
    | M4b
    deriving (Show, Read, Eq, Enum, Generic)

instance FromJSON AudioFormat

supportedAudioFormats :: [AudioFormat]
supportedAudioFormats = enumFrom $ toEnum 0

audioFileExtension :: AudioFormat -> String
audioFileExtension = fmap C.toLower <$> show

filePathAudioFormat :: FilePath -> Maybe AudioFormat
filePathAudioFormat fp = L.find (correctExtension . audioFileExtension) supportedAudioFormats
    where
        correctExtension = (== FP.takeExtension fp)

ffmpegFormatStr :: AudioFormat -> String
ffmpegFormatStr = audioFileExtension

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
        case L.find ((== t) . toDatabaseValue) allCalibreBookFormats of
            Just format -> Ok format
            Nothing     -> returnError ConversionFailed f $ T.unpack $ t <> " isn't a supported book format"
    fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance FromBackendRow Sqlite CalibreBookFormat

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be CalibreBookFormat where
    sqlValueSyntax = sqlValueSyntax . toDatabaseValue

instance (IsSql92ExpressionSyntax be) => HasSqlEqualityCheck be CalibreBookFormat
