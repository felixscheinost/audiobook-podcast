{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SendFile (
    sendFileMime,
    sendFileConduit,
    sendFileMimeConduit
) where

import qualified Data.ByteString.Builder as BSB
import           Data.Conduit            (Flush (..))
import           Data.Conduit.Binary     (sourceFileRange)
import qualified Data.Text               as T
import           Data.Time.Clock         (getCurrentTime)
import           Database                (Audiobook, AudiobookT (..))
import qualified Database                as DB
import           Import                  hiding (count, fileSize)
import qualified Library
import qualified Network.HTTP.Types      as HTTP
import           Network.Mime            (defaultMimeLookup)
import           System.FilePath         (takeFileName)
import           System.IO               (IOMode (ReadMode))
import           Yesod.RssFeed

rangeNotSatisfiable :: Handler a
rangeNotSatisfiable = sendResponseStatus status416 ("" :: Text)

type FileSize = Integer

-- Checks that a byte range is valid
--  - if yes: set Content-Range header and return status code 206 in third return value
--  - if no: respond with status code 416
checkRange :: FileSize -> Integer -> Integer -> Handler (Integer, Integer, Status)
checkRange fileSize from to
    | 0 <= from && from <= to && to <= fileSize= do
        replaceOrAddHeader "Content-Range" $ tshow from ++ "-" ++ tshow to ++ "/" ++ tshow fileSize
        return (from, to, status206)
    | otherwise = rangeNotSatisfiable

-- Handles the HTTP `Range` header.
-- Multiple ranges not supported and yield a 416.
-- Sets the "Content-Range" responds header if the range is valid
-- Converts the byte range to a byte offset and number of bytes to send
-- Also returns the status code the client should set:
--  - 200 if no range header was present
--  - 206 if a valid range header was present
parseRange :: FileSize -> Handler (Integer, Integer, Status)
parseRange fileSize = do
    replaceOrAddHeader "Accept-Ranges" "bytes"
    range <- lookupHeader "Range"
    let noRangeHeader = return (0, fileSize - 1, status200)
    let handleParseResult r = case r of
            Just [ByteRangeFrom from] ->
                checkRange fileSize from (fileSize - 1)
            Just [ByteRangeFromTo from to] ->
                checkRange fileSize from to
            Just [ByteRangeSuffix to] ->
                checkRange fileSize (fileSize - to - 1) (fileSize - 1)
            _ ->
                rangeNotSatisfiable
    (from, to, status) <- maybe noRangeHeader (handleParseResult . HTTP.parseByteRanges) range
    let count = to - from + 1
    replaceOrAddHeader "Content-Length" $ tshow count
    return (from, count, status)

-- Respond with a file; Take the mime type from the file extension.
-- This uses the sendfile(2) call on Linux.
sendFileMime :: FilePath -> Handler TypedContent
sendFileMime fp = do
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    sendFile mime fp

-- Had sporadic problems with high CPU (probAudiobookly GC) using `sendFile`
-- This sends the file manually using a conduit
-- => Slower than using sendfile(2)
--   => TODO: Switch back to `sendFile`
sendFileConduit :: ContentType -> FilePath -> Handler TypedContent
sendFileConduit mime fp = do
    fileSize <- withFile fp ReadMode hFileSize
    notFFmpeg <- (/= Just "calibre_ffmpeg") <$> lookupHeader "User-Agent"
    -- even if I completely turned off all range logic, just the "Accept-Ranges" header.
    -- resulted in an ffmpeg error. Turn off ranges completely for ffmpeg.
    (offset, count, status) <-
        if notFFmpeg then parseRange fileSize
        else return (0, fileSize, status200)
    sendResponseStatus status $ TypedContent mime $ ContentSource $
        mapOutput (Chunk . BSB.byteString) $
            sourceFileRange fp (Just offset) (Just count)

sendFileMimeConduit :: FilePath -> Handler TypedContent
sendFileMimeConduit fp = do
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    sendFileConduit mime fp
