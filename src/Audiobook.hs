{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Audiobook where

import           Conduit                 (ConduitT)
import qualified Conduit                 as CDT
import           Control.Monad.Catch     (throwM)
import           Data.Char               (toLower)
import           Data.Conduit.Process    (ClosedStream (..), Inherited (..),
                                          proc, streamingProcess)
import           Data.Either             (either)
import qualified Data.Text               as T
import           Database.Calibre        (BookAndData, bookFullPath, bookId)
import           Database.Calibre.Tables (dataFormat)
import qualified Database.Calibre.Types  as C
import           Import                  hiding (toLower)
import           System.FilePath         (takeExtension)
import qualified System.IO.Temp          as TMP
import           Zip                     as Z

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
                        (files, []) -> return $ Right $ Zip (snd x) fullPath (map fst files)
                        _ -> return $ Left ZipContainsMultipleFormats
        _     -> return $ Left UnsupportedFormat

getAudiobookMp3 :: BookAndData -> Handler (ConduitT () ByteString Handler ())
getAudiobookMp3 book = do
    audiobookType <- getAudiobookType book >>= either throwM return
    case audiobookType of
        SingleFile Mp3 filePath   -> return $ CDT.sourceFile filePath
        Zip Mp3 _ files -> do
            urlRender <- getUrlRender
            let urls = map (urlRender . BookRawFileR (bookId $ fst book) . T.pack) files
            let escape = T.replace "'" "'\''"
            let lineFor url = T.concat ["file '", escape url, "'"]
            let fileContents = T.intercalate "\n" $ map lineFor urls
            tempFile <- liftIO $ TMP.writeSystemTempFile "calibre-audiobook-ffmpeg" $ T.unpack fileContents
            let cmd = proc "ffmpeg" 
                    [ "-f", "concat"
                    , "-safe", "0"
                    , "-protocol_whitelist", "file,tcp,http"
                    , "-i", tempFile
                    , "-c", "copy"
                    , "-f", "mp3"
                    , "-"
                    ]
            -- todo: handle non-zero return code as HTTP error
            liftIO $ print tempFile 
            (ClosedStream, fromProcess, Inherited, _) <- liftIO $ streamingProcess cmd
            return fromProcess



--ffmpegJoinFiles :: [String] -> Handler (Either String (ConduitT i ByteString Handler ()))
--ffmpegJoinFiles files
