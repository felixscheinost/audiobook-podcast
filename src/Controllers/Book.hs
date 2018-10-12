{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controllers.Book where

import           Audiobook
import qualified Data.Binary.Builder as BSB
import           Data.Conduit        (Flush (..))
import qualified Data.Text           as T
import           Database.Calibre
import           Import
import           Network.Mime        (defaultMimeLookup)
import           System.FilePath     (takeFileName)
import           Zip                 (getSingleFile)

getBook :: Int -> Handler BookAndData
getBook _id = runSQL (getAudiobook _id) >>= maybe notFound return

sendFileMime :: FilePath -> Handler TypedContent
sendFileMime = sendFile <$> defaultMimeLookup . T.pack . takeFileName <*> id

getBookCoverR :: Int -> Handler TypedContent
getBookCoverR _id = getBook _id >>= bookCover >>= sendFileMime

getBookRawFileR :: Int -> Text -> Handler TypedContent
getBookRawFileR _id zipFilePath = do
    abType <- getBook _id 
        >>= getAudiobookType
        >>= either (invalidArgs . (:[]) . T.pack . show) return
    case abType of
        SingleFile _ filePath ->
            sendFileMime filePath
        Zip _ zipPath _ -> do
            let mime = defaultMimeLookup zipFilePath
            conduit <- liftIO $ getSingleFile zipPath (T.unpack zipFilePath)
            respondSource mime $ mapOutput (Chunk . BSB.fromByteString) conduit

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
                <button .close type="button" data-dismiss="modal" aria-label="Close">
                    <span aria-hidden="true">&times;
            <div .modal-body.row>
                <div .col-md-3>
                    <img style="height: 250px" src=@{BookCoverR _id}>
                <div .col-md-9>
                    <h4> #{ bookTitle $ fst book }
                    <a .btn.btn-primary href="#"> Copy RSS link
                    <a .btn.btn-primary href=@{BookMp3FileR _id}> Download MP3
    |]
