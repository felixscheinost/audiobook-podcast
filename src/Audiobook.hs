{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Audiobook(
    AudioFormat(..),
    AudiobookType(..),
    AudiobookError(..),
    getAudiobookType, getAudiobookMp3
) where

import           Conduit                       (ConduitT)
import qualified Conduit                       as CDT
import           Control.Monad.Catch           (throwM)
import qualified Data.ByteString               as B
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.Char                     (toLower)
import           Data.Conduit.Process          (ClosedStream (..),
                                                CreateProcess (std_err),
                                                StdStream (..),
                                                UseProvidedHandle (..), proc,
                                                streamingProcess,
                                                streamingProcessHandleRaw,
                                                terminateProcess)
import           Data.Either                   (either)
import qualified Data.Text                     as T
import           Database.Calibre              (bookFullPath, bookId)
import qualified Database.Calibre.BookFormat   as DF
import           Database.Calibre.Tables       (BookAndData (..), dataFormat)
import           Import                        hiding (toLower)
import qualified System.Directory              as SYSDIR
import           System.FilePath               (takeExtension)
import qualified System.IO.Temp                as TMP
import           Zip                           as Z

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

getAudiobookType :: BookAndData -> Handler (Either AudiobookError AudiobookType)
getAudiobookType book@BookAndData{..} =
    case dataFormat bdData of
        DF.MP3 -> Right . SingleFile Mp3 <$> bookFullPath book
        DF.M4A -> Right . SingleFile M4a <$> bookFullPath book
        DF.M4B -> Right . SingleFile M4b <$> bookFullPath book
        DF.ZIP -> do
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
getAudiobookMp3 book@BookAndData{..} = do
    audiobookType <- getAudiobookType book >>= either throwM return
    case audiobookType of
        SingleFile Mp3 filePath -> return $ CDT.sourceFile filePath
        SingleFile sourceFormat _ -> do
            urlRender <- getUrlRender
            mp3Quality <- appMp3Quality . appSettings <$> getYesod
            let fileUrl = urlRender $ BookRawFileR (bookId bdBook)
            let ffmpegArgs = [ "-f", ffmpegFormatStr sourceFormat
                             , "-user_agent", "calibre_ffmpeg" -- for disabling HTTP Range handling
                             , "-i", T.unpack fileUrl
                             , "-seekable", "0"
                             , "-f", "mp3"
                             , "-q:a", show mp3Quality
                             , "-vn" -- no video
                             , "-"
                             ]
            liftIO $ ffmpeg ffmpegArgs
        Zip Mp3 _ files -> do
            urlRender <- getUrlRender
            mp3Quality <- appMp3Quality . appSettings <$> getYesod
            let urls = map (urlRender . BookRawFileZipR (bookId bdBook) . T.pack) files
            let lineFor url = T.concat ["file '", url, "'"]
            let fileContents = T.intercalate "\n" $ map lineFor urls
            let setup = do
                    tempFileInput <- TMP.writeSystemTempFile "calibre-audiobook-ffmpeg-input" $ T.unpack fileContents
                    let ffmpegArgs = [ "-f", "concat"
                                     , "-safe", "0"
                                     , "-protocol_whitelist", "file,tcp,http"
                                     , "-i", tempFileInput
                                     , "-f", "mp3"
                                     , "-q:a", show mp3Quality
                                     , "-vn"
                                     , "-"
                                     ]
                    ffmpegStdout <- ffmpeg ffmpegArgs
                    return (ffmpegStdout, tempFileInput)
            return $ CDT.bracketP
                setup
                (SYSDIR.removeFile . snd)
                fst

-- added this to always try to fill a chunk (if possible)
sourceHandleNoSome :: MonadIO m => Handle -> ConduitT i ByteString m ()
sourceHandleNoSome h =
    loop
    where
    loop = do
        bs <- liftIO (B.hGet h defaultChunkSize)
        if B.null bs
            then return ()
            else yield bs >> loop

ffmpeg :: [String] -> IO (ConduitT ()  ByteString Handler ())
ffmpeg args = do
    let setup = do
            let cmd = (proc "ffmpeg" args) { std_err = NoStream }
            (ClosedStream, stdoutHandle, UseProvidedHandle, streamingHandle) <- liftIO $ streamingProcess cmd
            return (sourceHandleNoSome stdoutHandle, streamingHandle)
    -- TODO: handle non-zero return code as HTTP error
    return $ CDT.bracketP
        setup
        (terminateProcess . streamingProcessHandleRaw . snd)
        fst
