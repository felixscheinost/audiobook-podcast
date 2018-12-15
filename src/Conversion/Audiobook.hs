{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Conversion.Audiobook where

import           Conversion.Step
import           Database.Calibre.Audiobook (Audiobook)
import           Database.Calibre.Format    (AudioFormat)
import           Import.NoFoundation
import qualified System.IO.Temp             as Temp
import qualified Zip

data FormatError
    = NoAudioFilesInZip
    | ZipContainsMultipleFormats
    | BookAlreadyContainsTargetFormat

data ConversionType
    = ZipConvert Audiobook FilePath AudioFormat AudioFormat
    | FileConvert Audiobook FilePath AudioFormat AudioFormat

rankConversionType :: ConversionType -> Int
rankConversionType (ZipConvert _ _ f t)
    | f == t                        = 1
    | otherwise                     = 3
rankConversionType FileConvert{}    = 2

-- conversionTypeForBookAndData :: (CalibreBook, CalibreBookData) -> ExceptT FormatError Handler ConversionType
-- conversionTypeForBookAndData bd@(book, bookData) = do
--     targetFormat <- lift $ appConversionTargetFormat . appSettings <$> getYesod
--     ab <- lift $ AB.calibreBookToAudiobook book
--     path <- lift $ DB.formatDataFullPath bd
--     case DB.dataFormat bookData of
--         Audio format -> do
--             when (format == targetFormat) $ throwE BookAlreadyContainsTargetFormat
--             return $ FileConvert ab path format targetFormat
--         ZIP -> do
--             filesWithFormats <- mapMaybe (\fp -> (fp,) <$> filePathAudioFormat fp) <$> Zip.getFilePathsInZip path
--             let (files, formats) = List.unzip filesWithFormats
--             format <- maybe (throwE NoAudioFilesInZip) (return . head) $ fromNullable formats
--             when (List.any (/= format) formats) $ throwE ZipContainsMultipleFormats
--             return $ ZipConvert ab path format targetFormat


-- | Describe a single step of the process
data StepDescription
    = UnpackZIP
    | Convert Int AudioFormat AudioFormat
    | Move

conversionSourceFile :: ConversionType -> IO (Step () (ConversionType, [FilePath]) Zip.UnzipProgress StepDescription)
conversionSourceFile ct@(ZipConvert _ zipPath _ _) = createStep (const $ Just UnpackZIP) $ \updateProgress _ -> do
    tempDir <- liftIO Temp.getCanonicalTemporaryDirectory
    files <- liftIO $ Zip.unpackInto tempDir zipPath updateProgress
    return (ct, files)
conversionSourceFile ct@(FileConvert _ path _ _) = createStep (const Nothing) $ \_ _ -> return (ct, [path])

conversionStep :: IO (Step (ConversionType, [FilePath]) FilePath Double StepDescription)
conversionStep = createStep description $ \updateProgress ct -> do
    putStrLn "conversion"
    return ""
        where
            description (ZipConvert _ _ f t, fps) = Just $ Convert (length fps) f t
            description (FileConvert _ _ f t, _) = Just $ Convert 1 f t

data Conversion
    = ConversionFormatError Audiobook FormatError
    | forall o p. StepProgress p => Conversion Audiobook (Step () o p StepDescription)
