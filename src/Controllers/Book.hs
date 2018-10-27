{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controllers.Book where

import           Audiobook
import qualified Data.Binary.Builder as BSB
import           Data.Conduit        (Flush (..))
import           Data.Conduit.Binary (sourceFileRange)
import qualified Data.Text           as T
import           Database.Calibre
import           Import hiding (count, fileSize)  
import qualified Network.HTTP.Types  as HTTP
import           Network.Mime        (defaultMimeLookup)
import           System.FilePath     (takeFileName)
import           System.IO           (IOMode (ReadMode))
import           Zip                 (getSingleFile)
import Yesod.RssFeed
import Data.Time.Clock (getCurrentTime)

getBook :: Int -> Handler BookAndData
getBook _id = runSQL (getAudiobook _id) >>= maybe notFound return

rangeNotSatisfiable :: Handler a
rangeNotSatisfiable = sendResponseStatus status416 ("" :: Text)

type FileSize = Integer

-- Checks that a byte range is valid 
--  - if yes: set Content-Range header and return status code 206 in third return value
--  - if no: respond with status code 416
checkRange :: FileSize -> Integer -> Integer -> Handler (Integer, Integer, Status)
checkRange fileSize from to
    | 0 <= from && from <= to && to <= fileSize= do
        replaceOrAddHeader "Content-Range" $ T.pack $ show from ++ "-" ++ show to ++ "/" ++ show fileSize
        return (from, to, status206)
    | otherwise = rangeNotSatisfiable

-- Handles the HTTP `Range` header.
-- Multiple ranges not supported and yield a 416.
-- Sets the "Content-Range" responds header if the range is valid
-- Converts the byte range to a byte offset and number of bytes to send
-- Also returns the status code the client should set:
--  - 200 if no range header was present
--  - 206 if a valid range header was present
handleRange :: FileSize -> Maybe ByteRanges -> Handler (Integer, Integer, Status)
handleRange fileSize maybeByteRange = do
    range <- lookupHeader "Range"
    putStrLn $ T.pack $ "Range: " ++ show range
    (from, to, status) <- case maybeByteRange of
        Just [ByteRangeFrom from] ->
            checkRange fileSize from (fileSize - 1)
        Just [ByteRangeFromTo from to] ->
            checkRange fileSize from to
        Just [ByteRangeSuffix to] ->
            checkRange fileSize (fileSize - to) (fileSize - 1)
        Nothing -> 
            return (0, fileSize - 1, status200)
        _ -> 
            rangeNotSatisfiable
    return (from, to - from + 1, status)

-- get range header, parse range header and do range handling using `parseRange` above
parseRange :: FileSize -> Handler (Integer, Integer, Status)
parseRange fs = (maybe Nothing HTTP.parseByteRanges <$> lookupHeader "Range") >>= handleRange fs

-- Respond with a file; Take the mime type from the file extension.
-- This uses the sendfile(2) call on Linux.
-- Had sporadic problems with high CPU (probably GC)
-- Switched to custom function `sendFileMimeConduit` which reads and sends the file manually
-- => Slower than using sendfile(2) 
sendFileMime :: FilePath -> Handler TypedContent
sendFileMime fp = do
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    sendFile mime fp

sendFileMimeConduit :: FilePath -> Handler TypedContent
sendFileMimeConduit fp = do
    fileSize <- withFile fp ReadMode hFileSize
    (offset, count, status) <- parseRange fileSize
    putStrLn $ T.pack $ show "Sending " ++ show count ++ " bytes, from " ++ show offset
    replaceOrAddHeader "Accept-Ranges" "bytes"
    replaceOrAddHeader "Content-Length" $ T.pack $ show count
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    sendResponseStatus status $ TypedContent mime $ ContentSource $
        mapOutput (Chunk . BSB.fromByteString) $
            sourceFileRange fp (Just offset) (Just count)

getBookCoverR :: Int -> Handler TypedContent
getBookCoverR _id = getBook _id >>= bookCover >>= sendFileMime

getBookRawFileR :: Int -> Handler TypedContent
getBookRawFileR _id = do
    abType <- getBook _id
        >>= getAudiobookType
        >>= either (invalidArgs . (:[]) . T.pack . show) return
    case abType of
        SingleFile _ filePath ->
            sendFileMimeConduit filePath
        _ ->
            invalidArgs ["Not a single file"]

getBookRawFileZipR :: Int -> Text -> Handler TypedContent
getBookRawFileZipR _id zipFilePath = do
    abType <- getBook _id
        >>= getAudiobookType
        >>= either (invalidArgs . (:[]) . T.pack . show) return
    case abType of
        Zip _ zipPath _ -> do
            let mime = defaultMimeLookup zipFilePath
            conduit <- liftIO $ getSingleFile zipPath (T.unpack zipFilePath)
            respondSource mime $ mapOutput (Chunk . BSB.fromByteString) conduit
        _ ->
            invalidArgs ["Not a ZIP file"]

getBookMp3FileR :: Int -> Handler TypedContent
getBookMp3FileR _id = do
    c <- getBook _id >>= getAudiobookMp3
    replaceOrAddHeader "Accept-Ranges" "none"
    respondSource "audio/mpeg" $ mapOutput (Chunk . BSB.fromByteString) c

getBookOverlayR :: Int -> Handler Html
getBookOverlayR _id = do
    book <- getBook _id
    defaultLayout [whamlet|
        <div .modal-content>
            <div .modal-header>
                <h5 .modal-title> #{ bookTitle $ fst book} (#{bookId $ fst book})
                <button .close type="button" data-dismiss="modal" aria-label="Close">
                    <span aria-hidden="true">&times;
            <div .modal-body .book-modal>
                <img src=@{BookCoverR _id}>
                <div>
                    <div .row>
                        <a .btn.btn-primary href=@{BookRssR _id}> _{MsgCopyRSSLink}
                        <a .btn.btn-primary href=@{BookMp3FileR _id}> _{MsgDownloadMP3}
    |]

bookFeed :: UTCTime -> CalibreBook -> Feed (Route App)
bookFeed now book = Feed 
    { feedTitle = bookTitle book
    , feedLinkSelf = BookRssR _id
    , feedLinkHome = BookRssR _id
    , feedAuthor = ""
    , feedDescription = ""
    , feedLanguage = "en"
    , feedUpdated = now
    , feedLogo = Just (BookCoverR _id, bookTitle book)
    , feedEntries = [ 
        FeedEntry 
        { feedEntryLink = BookRssR _id
        , feedEntryUpdated = now
        , feedEntryTitle = bookTitle book
        , feedEntryContent = ""
        , feedEntryEnclosure = Just $ EntryEnclosure
            { enclosedUrl = BookMp3FileR _id
            , enclosedSize = 0
            , enclosedMimeType = "audio/mpeg"
            }
        } 
    ]
    }
    where
        _id = bookId book

getBookRssR :: Int -> Handler RepRss
getBookRssR _id = do
    (b, _) <- getBook _id
    now <- liftIO getCurrentTime
    rssFeed $ bookFeed now b