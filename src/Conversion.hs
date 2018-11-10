{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}

module Conversion where

import qualified Algorithms.NaturalSort      as NaturalSort
import           Audiobook                   (Audiobook)
import qualified Audiobook                   as AB
import           Control.Monad               (liftM2)
import           Control.Monad.Trans.Except  (ExceptT, throwE)
import qualified Data.List                   as List
import           Database.Calibre            (CalibreBook, CalibreBookData)
import qualified Database.Calibre            as DB
import           Database.Calibre.BookFormat (AudioFormat,
                                              CalibreBookFormat (Audio, ZIP),
                                              filePathAudioFormat)
import           Import
import           System.IO.Temp              (getCanonicalTemporaryDirectory)
import qualified Zip

-- | The different cases the conversion process needs to handle
-- In decending order preference
data ConversionType
    = ZipTargetFormat Audiobook FilePath
    | DifferentFormat Audiobook AudioFormat FilePath
    | ZipDifferentFormat Audiobook AudioFormat FilePath

rankConversionType :: ConversionType -> Int
rankConversionType ZipTargetFormat{}    = 1
rankConversionType DifferentFormat{}    = 2
rankConversionType ZipDifferentFormat{} = 3

data ConversionTypeError
    = NoAudioFilesInZip
    | ZipContainsMultipleFormats
    | BookAlreadyContainsTargetFormat

convertionTypeErrorMsg :: ConversionTypeError -> AppMessage
convertionTypeErrorMsg NoAudioFilesInZip          = MsgNoAudioFilesInZip
convertionTypeErrorMsg ZipContainsMultipleFormats = MsgZipContainsMultipleFormats
convertionTypeErrorMsg BookAlreadyContainsTargetFormat = MsgBookAlreadyContainsTargetFormat

conersionTypeForBookAndData :: (CalibreBook, CalibreBookData) -> ExceptT ConversionTypeError Handler ConversionType
conersionTypeForBookAndData bd@(book, bookData) = do
    targetFormat <- lift $ appConversionTargetFormat . appSettings <$> getYesod
    ab <- lift $ AB.calibreBookToAudiobook book
    path <- lift $ DB.formatDataFullPath bd
    case DB.dataFormat bookData of
        Audio format -> do
            when (format == targetFormat) $ throwE BookAlreadyContainsTargetFormat
            return $ DifferentFormat ab format path
        ZIP -> do
            formats <- mapMaybe filePathAudioFormat <$> Zip.getFilePathsInZip path
            format <- maybe (throwE NoAudioFilesInZip) (return . head) $ fromNullable formats
            when (List.any (/= format) formats) $ throwE ZipContainsMultipleFormats
            if format == targetFormat
                then return $ ZipTargetFormat ab path
                else return $ ZipDifferentFormat ab format path

-- | Describe a single step of the process
data StepDescription
    = UnpackZIP
    | Convert Int AudioFormat AudioFormat
    | Move

class StepProgress a where
    asPercentDouble :: a -> Double

-- instance StepProgress (Integer, Integer) where
--     asPercentDouble (delta, total) = 100 * fromIntegral delta / fromIntegral total

-- instance StepProgress Double where
--     asPercentDouble = id

-- | State of the step
data StepState a
    = Waiting
    | Running (TVar (Maybe a))
    | Finished

-- | Step action that takes i as input, produces o as output and has progress information p
type StepAction i o p =  ((Maybe p -> Maybe p) -> IO ()) -> i -> IO o

-- | Holds the data for one conversion step
data StepData i o id od p = StepData
    { action      :: StepAction i o p
    , description :: id -> IO (Maybe StepDescription, od)
    , state       :: TVar (StepState p)
    }

-- | Conversion step that takes i and produces o, for description takes id and produces od for next description
data Step i o id od p
    = Single (StepData i o id od p)
    | forall u ud p'. Nested (StepData i u id ud p) (Step u o ud od p')

-- | Run two steps after each other
andThen :: Step i u id ud p -> Step u o ud od p' -> Step i o id od p
andThen (Single c) next   = Nested c next
andThen (Nested c d) next = Nested c (andThen d next)

andThenM :: Monad m => m (Step i u id ud p) -> m (Step u o ud od p') -> m (Step i o id od p)
andThenM = liftM2 andThen

-- | Convert an action to a step
fromIO :: (id -> IO (Maybe StepDescription, od)) -> StepAction i o p -> IO (Step i o id od p)
fromIO desc action = do
    m <- atomically $ newTVar Waiting
    return $ Single $ StepData
        { action = action
        , description = desc
        , state = m
        }

-- | An action that produces a constant output
const :: o -> od -> IO (Step i o id od p)
const output outputDescription = do
    let descriptionFor input = return (Nothing, outputDescription)
    let action updateProgress input = return output
    fromIO descriptionFor action

-- | An action that unzips a file to a temporary location and returns the paths to the unzipped files when run.
--   A temporary folder is created into which each file is unzipped using its original name.
unzip :: IO (Step FilePath [FilePath] FilePath [FilePath] Zip.UnzipProgress)
unzip = fromIO
    (fmap ((,) (Just UnpackZIP)) . Zip.getFilePathsInZip)
    $ \updateProgress zipPath -> do
        tempDir <- getCanonicalTemporaryDirectory
        Zip.unpackInto tempDir zipPath updateProgress
