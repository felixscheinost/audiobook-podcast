{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Audiobook(
    listAudiobooks,
    listAudiobooksInSeries,
    getAudiobook,
    listSeries,
    Audiobook(..),
) where

import qualified Data.Maybe                  as M
import           Database.Calibre            (BookAndData, CalibreSeries)
import qualified Database.Calibre            as DB
import           Database.Calibre.BookFormat (CalibreBookFormat (Audio, ZIP))
import           Import

data Audiobook = Audiobook
    { abId     :: Int
    , abTitle  :: Text
    , abPath   :: FilePath
    , abFormat :: CalibreBookFormat
    , abCover  :: FilePath
    }

listAudiobooks :: Handler [Audiobook]
listAudiobooks = runSQL DB.listBooks >>= mapM calibreBookToAudiobook

listAudiobooksInSeries :: Int -> Handler [Audiobook]
listAudiobooksInSeries _seriesId = runSQL (DB.listBooksInSeries _seriesId) 
        >>= mapM calibreBookToAudiobook

listSeries :: Handler [(CalibreSeries, Text)]
listSeries = runSQL DB.listSeries

getAudiobook :: Int -> Handler Audiobook
getAudiobook _id = runSQL (DB.getBook _id) >>= maybe notFound calibreBookToAudiobook

calibreBookToAudiobook :: BookAndData -> Handler Audiobook
calibreBookToAudiobook bd@(book, bookData) = do
    let abId = DB.bookId book
    let abTitle = DB.bookTitle book
    abPath <- DB.bookFullPath bd
    let abFormat = DB.dataFormat bookData
    abCover <- DB.bookCover bd
    return Audiobook{..}

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
