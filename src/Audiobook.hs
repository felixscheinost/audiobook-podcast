{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Audiobook where

import           Conduit                 (ConduitT)
import qualified Conduit                 as CDT
import           Data.Char               (toLower)
import           Data.Maybe              (catMaybes, isJust)
import           Database.Calibre        (BookAndData, bookFullPath)
import           Database.Calibre.Tables (dataFormat)
import qualified Database.Calibre.Types  as C
import           Import                  hiding (toLower)
import           System.FilePath         (takeExtension)
import           Zip                     as Z
import Control.Monad.Catch (throwM)

data AudioFormat
    = Mp3
    | M4a
    deriving (Show, Read, Eq, Enum)

supportedAudioFormats :: [AudioFormat]
supportedAudioFormats = enumFrom (toEnum 0)

fileExtension :: AudioFormat -> String
fileExtension = ('.' :) . map toLower . show

supportedAudioFormat :: FilePath -> Maybe AudioFormat
supportedAudioFormat fp = find ((== takeExtension fp) . fileExtension) supportedAudioFormats

toAudioFile :: FilePath -> Maybe (FilePath, AudioFormat)
toAudioFile f = (f,) <$> supportedAudioFormat f

data AudiobookType
    = Zip AudioFormat FilePath [FilePath]
    | SingleFile AudioFormat FilePath
    deriving (Eq)

data AudiobookError 
    = NoSupportedAudiobooksInZip
    | ZipContainsMultipleFormats
    | UnsupportedFormat

instance Show AudiobookError where
    show NoSupportedAudiobooksInZip = "ZIP file doesn't contain any supported audio files"
    show ZipContainsMultipleFormats = "ZIP contains files from multiple supported formats"
    show UnsupportedFormat          = "Unsupported format"

instance Exception AudiobookError

getAudiobookType :: BookAndData -> Handler (Either AudiobookError AudiobookType )
getAudiobookType book =
    case dataFormat $ snd book of
        C.MP3 -> Right . SingleFile Mp3 <$> bookFullPath book
        C.M4A -> Right . SingleFile M4a <$> bookFullPath book
        C.ZIP -> do
            fullPath <- bookFullPath book
            zipAudioFiles <- mapMaybe toAudioFile <$> Z.getFiles fullPath
            case zipAudioFiles of
                []     -> return $ Left NoSupportedAudiobooksInZip
                (x:xs) ->
                    case partition ((== snd x) . snd) xs of
                        -- make sure ZIP contains only supported files of one format
                        (files, []) -> return $ Right $ Zip (snd x) fullPath (map fst (x:xs))
                        _ -> return $ Left ZipContainsMultipleFormats
        _     -> return $ Left UnsupportedFormat 

getAudiobookMp3 :: BookAndData -> Handler (ConduitT () ByteString Handler ())
getAudiobookMp3 book = do
    audiobookTypeEither <- getAudiobookType book
    audiobookType <- case audiobookTypeEither of 
        Left err -> throwM err
        Right abType -> return abType
    case audiobookType of
        SingleFile Mp3 path -> return $ CDT.sourceFile path

