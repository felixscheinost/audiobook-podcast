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

getBook :: Int -> Handler BookAndData
getBook _id = runSQL (getAudiobook _id) >>= maybe notFound return

rangeNotSatisfiable :: Handler a
rangeNotSatisfiable = sendResponseStatus status416 ("" :: Text)

type OffsetAndCount = (Integer, Integer)

checkRange :: Integer -> OffsetAndCount -> Handler OffsetAndCount
checkRange fileSize (offset, count)
    | offset <= fileSize && count >= 0 = return (offset, min maxCount count)
    | otherwise = rangeNotSatisfiable
    where
        maxCount = fileSize - offset

sendFileMime :: FilePath -> Handler TypedContent
sendFileMime fp = do
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    sendFile mime fp

sendFileMimeConduit :: FilePath -> Handler TypedContent
sendFileMimeConduit fp = do
    fileSize <- withFile fp ReadMode hFileSize
    maybeByteRange <-  (>>= HTTP.parseByteRanges) <$> lookupHeader "Range"
    (offset, count) <- case maybeByteRange of
            Just [ByteRangeFrom from] ->
                checkRange fileSize (from, fileSize - from)
            Just [ByteRangeFromTo from to] ->
                checkRange fileSize (from, to - from + 1)
            Just [ByteRangeSuffix to] ->
                checkRange fileSize (0, to + 1)
            Nothing -> return (0, fileSize)
            _ -> rangeNotSatisfiable
    replaceOrAddHeader "Content-Length" $ T.pack $ show count
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    respondSource mime $
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
                        <a .btn.btn-primary href="#"> _{MsgCopyRSSLink}
                        <a .btn.btn-primary href=@{BookMp3FileR _id}> _{MsgDownloadMP3}
    |]
