{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Audiobook where

import           Data.Char               (toLower)
import           Data.Maybe              (catMaybes, isJust)
import           Database.Calibre        (BookAndData, bookFullPath)
import           Database.Calibre.Tables (dataFormat)
import qualified Database.Calibre.Types  as C
import           Import                  hiding (toLower)
import           System.FilePath         (takeExtension)
import           Zip                     as Z

data AudioFormat =
    Mp3
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

getAudiobookType :: BookAndData -> Handler (Either String AudiobookType)
getAudiobookType book =
    case dataFormat $ snd book of
        C.MP3 -> Right . SingleFile Mp3 <$> bookFullPath book
        C.ZIP -> do
            fullPath <- bookFullPath book
            zipAudioFiles <- mapMaybe toAudioFile <$> Z.getFiles fullPath
            case zipAudioFiles of
                []     -> return $ Left "ZIP doesn't contain any supported audio files"
                (x:xs) ->
                    case partition ((== snd x) . snd) xs of
                        -- make sure ZIP contains only supported files of one format
                        (files, []) -> return $ Right $ Zip (snd x) fullPath (map fst (x:xs))
                        _ -> return $ Left "ZIP contains files of different formats"



