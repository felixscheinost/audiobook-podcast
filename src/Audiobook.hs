{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Audiobook(
    listAudiobooks, getAudiobook,
    Audiobook(..)
) where

import Database.Calibre (BookAndData)
import Database.Calibre.BookFormat (CalibreBookFormat(ZIP, Audio))
import qualified Database.Calibre              as DB
import qualified Data.Maybe as M
import           Import                       

listAudiobooks :: Handler [Audiobook]
listAudiobooks = do
    mp3Books <- runSQL (DB.listBooks [Audio Mp3])
    M.catMaybes <$> mapM calibreBookToAudiobook mp3Books

getAudiobook :: Int -> Handler Audiobook
getAudiobook _id = do
    book <- runSQL (DB.getBook _id) >>= maybe notFound return
    calibreBookToAudiobook book >>= maybe (invalidArgs ["Not a Audiobook"]) return

calibreBookToAudiobook :: BookAndData -> Handler (Maybe Audiobook)
calibreBookToAudiobook bd = do
    let abId = Just $ DB.bookId $ DB.bdBook bd
    let abTitle = Just $ DB.bookTitle $ DB.bdBook bd
    abPath <- Just <$> DB.bookFullPath bd
    let abFormat = getAudioFormat $ DB.dataFormat $ DB.bdData bd
    abCover <- Just <$> DB.bookCover bd
    return $ Audiobook <$> abId <*> abTitle <*> abPath <*> abFormat <*> abCover

getAudioFormat :: CalibreBookFormat -> Maybe AudioFormat
getAudioFormat ZIP = Nothing
getAudioFormat (Audio f) = Just f

-- getAudiobookMp3 :: BookAndData -> Handler (ConduitT () ByteString Handler ())
-- getAudiobookMp3 book@BookAndData{..} = do
--     audiobookType <- getAudiobookType book >>= either throwM return
--     case audiobookType of
--         SingleFile Mp3 filePath -> return $ CDT.sourceFile filePath
--         SingleFile sourceFormat _ -> do
--             urlRender <- getUrlRender
--             mp3Quality <- appMp3Quality . appSettings <$> getYesod
--             let fileUrl = urlRender $ BookRawFileR (bookId bdBook)
--             let ffmpegArgs = [ "-f", ffmpegFormatStr sourceFormat
--                              , "-user_agent", "calibre_ffmpeg" -- for disabling HTTP Range handling
--                              , "-i", T.unpack fileUrl
--                              , "-seekable", "0"
--                              , "-f", "mp3"
--                              , "-q:a", show mp3Quality
--                              , "-vn" -- no video
--                              , "-"
--                              ]
--             liftIO $ ffmpeg ffmpegArgs
--         Zip Mp3 _ files -> do
--             urlRender <- getUrlRender
--             let urls = map (urlRender . BookRawFileZipR (bookId bdBook) . T.pack) files
--             let lineFor url = T.concat ["file '", url, "'"]
--             let fileContents = T.intercalate "\n" $ map lineFor urls
--             let setup = do
--                     tempFileInput <- TMP.writeSystemTempFile "calibre-audiobook-ffmpeg-input" $ T.unpack fileContents
--                     let ffmpegArgs = [ "-f", "concat"
--                                      , "-safe", "0"
--                                      , "-protocol_whitelist", "file,tcp,http"
--                                      , "-i", tempFileInput
--                                      , "-f", "mp3"
--                                      , "-c", "copy"
--                                      , "-vn"
--                                      , "-"
--                                      ]
--                     ffmpegStdout <- ffmpeg ffmpegArgs
--                     return (ffmpegStdout, tempFileInput)
--             return $ CDT.bracketP
--                 setup
--                 (SYSDIR.removeFile . snd)
--                 fst

-- -- added this to always try to fill a chunk (if possible)
-- sourceHandleNoSome :: MonadIO m => Handle -> ConduitT i ByteString m ()
-- sourceHandleNoSome h =
--     loop
--     where
--     loop = do
--         bs <- liftIO (B.hGet h defaultChunkSize)
--         if B.null bs
--             then return ()
--             else yield bs >> loop

-- ffmpeg :: [String] -> IO (ConduitT ()  ByteString Handler ())
-- ffmpeg args = do
--     let setup = do
--             let cmd = (proc "ffmpeg" args) { std_err = NoStream }
--             (ClosedStream, stdoutHandle, UseProvidedHandle, streamingHandle) <- liftIO $ streamingProcess cmd
--             return (sourceHandleNoSome stdoutHandle, streamingHandle)
--     -- TODO: handle non-zero return code as HTTP error
--     return $ CDT.bracketP
--         setup
--         (terminateProcess . streamingProcessHandleRaw . snd)
--         fst
