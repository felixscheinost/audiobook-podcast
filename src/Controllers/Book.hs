{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controllers.Book where

import           Audiobook
import qualified Data.Binary.Builder as BSB
import           Data.Conduit        (Flush (..))
import qualified Conduit        as CDT
import qualified Data.Text           as T
import           Database.Calibre
import           Import
import           Network.Mime        (defaultMimeLookup)
import qualified Network.Wai         as WAI
import           System.FilePath     (takeFileName)
import           System.IO           (IOMode (ReadMode))
import           Zip                 (getSingleFile)

getBook :: Int -> Handler BookAndData
getBook _id = runSQL (getAudiobook _id) >>= maybe notFound return

sendFileMime :: FilePath -> Handler TypedContent
sendFileMime fp = do
    fs <- withFile fp ReadMode hFileSize
    replaceOrAddHeader "Content-Length" $ T.pack $ show fs
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    respondSource mime $ mapOutput (Chunk . BSB.fromByteString) $ CDT.sourceFile fp

getBookCoverR :: Int -> Handler TypedContent
getBookCoverR _id = getBook _id >>= bookCover >>= sendFileMime


getBookRawFileR :: Int -> Handler TypedContent
getBookRawFileR _id = do
    abType <- getBook _id
        >>= getAudiobookType
        >>= either (invalidArgs . (:[]) . T.pack . show) return
    --WAI.requestHeaders <$> waiRequest >>= liftIO . print
    case abType of
        SingleFile _ filePath ->
            sendFileMime filePath
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
    withUrlRenderer [hamlet|
        <div .modal-content>
            <div .modal-header>
                <h5 .modal-title> #{ bookTitle $ fst book} (#{bookId $ fst book})
                <button .close type="button" data-dismiss="modal" aria-label="Close">
                    <span aria-hidden="true">&times;
            <div .modal-body .book-modal>
                <img src=@{BookCoverR _id}>
                <div>
                    <div .row>
                        <a .btn.btn-primary href="#"> Copy RSS link
                        <a .btn.btn-primary href=@{BookMp3FileR _id}> Download MP3
                    <p>Format of source file: #{ show $ dataFormat $ snd book }
    |]
