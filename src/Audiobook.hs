{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Audiobook where

import           Conduit                 (ConduitT)
import qualified Conduit                 as CDT
import           Control.Monad.Catch     (throwM)
import           Data.Char               (toLower)
import           Data.Conduit.Process    (ClosedStream (..),
                                          CreateProcess (std_err),
                                          StdStream (..),
                                          UseProvidedHandle (..), proc,
                                          streamingProcess,
                                          streamingProcessHandleRaw,
                                          terminateProcess)
import           Data.Either             (either)
import qualified Data.Text               as T
import           Database.Calibre        (BookAndData, bookFullPath, bookId)
import           Database.Calibre.Tables (dataFormat)
import qualified Database.Calibre.Types  as C
import           Import                  hiding (toLower)
import qualified System.Directory        as SYSDIR
import           System.FilePath         (takeExtension)
import qualified System.IO.Temp          as TMP
import           Zip                     as Z

data AudioFormat
    = Mp3
    | M4a
    | M4b
    deriving (Show, Read, Eq, Enum)

supportedAudioFormats :: [AudioFormat]
supportedAudioFormats = enumFrom (toEnum 0)

fileExtension :: AudioFormat -> String
fileExtension = ('.' :) . map toLower . show

ffmpegFormatStr :: AudioFormat -> String
ffmpegFormatStr = map toLower . show

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
        C.M4B -> Right . SingleFile M4b <$> bookFullPath book
        C.ZIP -> do
            fullPath <- bookFullPath book
            zipAudioFiles <- mapMaybe toAudioFile <$> Z.getFiles fullPath
            case zipAudioFiles of
                []     -> return $ Left NoSupportedAudiobooksInZip
                (x:xs) ->
                    case partition ((== snd x) . snd) xs of
                        -- make sure ZIP contains only supported files of one format
                        (_, []) -> return $ Right $ Zip (snd x) fullPath (map fst (x:xs))
                        _ -> return $ Left ZipContainsMultipleFormats

getAudiobookMp3 :: BookAndData -> Handler (ConduitT () ByteString Handler ())
getAudiobookMp3 book = do
    audiobookType <- getAudiobookType book >>= either throwM return
    case audiobookType of
        SingleFile Mp3 filePath -> return $ CDT.sourceFile filePath
        SingleFile sourceFormat _ -> do
            urlRender <- getUrlRender
            let fileUrl = urlRender $ BookRawFileR (bookId $ fst book)
            let ffmpegArgs = [ "-f", ffmpegFormatStr sourceFormat
                             , "-i", T.unpack fileUrl
                             , "-seekable", "0"
                             , "-f", "mp3"
                             , "-"
                             ]
            liftIO $ ffmpeg ffmpegArgs
        Zip Mp3 _ files -> do
            urlRender <- getUrlRender
            let urls = map (urlRender . BookRawFileZipR (bookId $ fst book) . T.pack) files
            let lineFor url = T.concat ["file '", url, "'"]
            let fileContents = T.intercalate "\n" $ map lineFor urls
            let setup = do
                    tempFileInput <- TMP.writeSystemTempFile "calibre-audiobook-ffmpeg-input" $ T.unpack fileContents
                    let ffmpegArgs = [ "-f", "concat"
                                     , "-safe", "0"
                                     , "-protocol_whitelist", "file,tcp,http"
                                     , "-i", tempFileInput
                                     , "-c", "copy"
                                     , "-f", "mp3"
                                     , "-"
                                     ]
                    ffmpegStdout <- ffmpeg ffmpegArgs
                    return (ffmpegStdout, tempFileInput)
            return $ CDT.bracketP
                setup
                (SYSDIR.removeFile . snd)
                fst

ffmpeg :: [String] -> IO (ConduitT ()  ByteString Handler ())
ffmpeg args = do
    let setup = do
            let cmd = (proc "ffmpeg" args) { std_err = NoStream }
            (ClosedStream, ffmpegStdout, UseProvidedHandle, streamingHandle) <- liftIO $ streamingProcess cmd
            return (ffmpegStdout, streamingHandle)
    -- TODO: handle non-zero return code as HTTP error
    return $ CDT.bracketP
        setup
        (terminateProcess . streamingProcessHandleRaw . snd)
        fst