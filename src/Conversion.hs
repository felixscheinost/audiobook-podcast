{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}

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

data ConversionType
    = ZipConvert Audiobook FilePath AudioFormat AudioFormat
    | FileConvert Audiobook FilePath AudioFormat AudioFormat

rankConversionType :: ConversionType -> Int
rankConversionType (ZipConvert _ _ f t)
    | f == t                        = 1
    | otherwise                     = 3
rankConversionType FileConvert{}    = 2

data ConversionError
    = NoAudioFilesInZip
    | ZipContainsMultipleFormats
    | BookAlreadyContainsTargetFormat

convertionTypeErrorMsg :: ConversionError -> AppMessage
convertionTypeErrorMsg NoAudioFilesInZip          = MsgNoAudioFilesInZip
convertionTypeErrorMsg ZipContainsMultipleFormats = MsgZipContainsMultipleFormats
convertionTypeErrorMsg BookAlreadyContainsTargetFormat = MsgBookAlreadyContainsTargetFormat

conersionTypeForBookAndData :: (CalibreBook, CalibreBookData) -> ExceptT ConversionError Handler ConversionType
conersionTypeForBookAndData bd@(book, bookData) = do
    targetFormat <- lift $ appConversionTargetFormat . appSettings <$> getYesod
    ab <- lift $ AB.calibreBookToAudiobook book
    path <- lift $ DB.formatDataFullPath bd
    case DB.dataFormat bookData of
        Audio format -> do
            when (format == targetFormat) $ throwE BookAlreadyContainsTargetFormat
            return $ FileConvert ab path format targetFormat
        ZIP -> do
            filesWithFormats <- mapMaybe (\fp -> (fp,) <$> filePathAudioFormat fp) <$> Zip.getFilePathsInZip path
            let (files, formats) = List.unzip filesWithFormats
            format <- maybe (throwE NoAudioFilesInZip) (return . head) $ fromNullable formats
            when (List.any (/= format) formats) $ throwE ZipContainsMultipleFormats
            return $ ZipConvert ab path format targetFormat

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
type StepAction i o p =  ((Maybe p -> Maybe p) -> IO ()) -> i -> ExceptT ConversionError IO o

-- | Holds the data for one conversion step
data StepData i o p = StepData
    { action      :: StepAction i o p
    , description :: i -> Maybe StepDescription
    , state       :: TVar (StepState p)
    }

-- | Conversion step that takes i and produces o, for description takes id and produces od for next description
data Step i o p
    = Single (StepData i o p)
    | forall u ud p'. Nested (StepData i u p) (Step u o p')

-- | Run two steps after each other
andThen :: Step i u p -> Step u o p' -> Step i o p
andThen (Single c) next   = Nested c next
andThen (Nested c d) next = Nested c (andThen d next)

andThenM :: Monad m => m (Step i u p) -> m (Step u o p') -> m (Step i o p)
andThenM = liftM2 andThen

-- | Convert an action to a step
fromIO :: (i -> Maybe StepDescription) -> StepAction i o p -> IO (Step i o p)
fromIO desc action = do
    m <- atomically $ newTVar Waiting
    return $ Single $ StepData
        { action = action
        , description = desc
        , state = m
        }

conversionSourceFile :: ConversionType -> IO (Step () (ConversionType, [FilePath]) Zip.UnzipProgress)
conversionSourceFile ct@(ZipConvert _ zipPath _ _) = fromIO (const $ Just UnpackZIP) $ \updateProgress _ -> do
    tempDir <- liftIO getCanonicalTemporaryDirectory
    files <- liftIO $ Zip.unpackInto tempDir zipPath updateProgress
    return (ct, files)
conversionSourceFile ct@(FileConvert _ path _ _) = fromIO (const Nothing) $ \_ _ -> return (ct, [path])

conversion :: IO (Step (ConversionType, [FilePath]) FilePath Double)
conversion = fromIO description $ \updateProgress ct -> do
    return ""
        where
            description (ZipConvert _ _ f t, fps) = Just $ Convert (length fps) f t
            description (FileConvert _ _ f t, _) = Just $ Convert 1 f t
