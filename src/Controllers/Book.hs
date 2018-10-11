{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Controllers.Book where

import           Audiobook
import qualified Codec.Archive.Zip   as Z
import qualified Data.Binary.Builder as BSB
import           Data.Conduit        (Flush (..))
import qualified Data.Text           as T
import           Database.Calibre
import           Import
import           Network.Mime        (defaultMimeLookup)
import           Prelude             ((!!))
import           System.FilePath     (takeFileName)
import           Zip                 (getSingleFile)

getBook :: Int -> Handler BookAndData
getBook id = runSQL (getAudiobook id) >>= maybe notFound return

sendFileMime :: FilePath -> Handler TypedContent
sendFileMime = sendFile <$> defaultMimeLookup . T.pack . takeFileName <*> id

getBookCoverR :: Int -> Handler TypedContent
getBookCoverR id = getBook id >>= bookCover >>= sendFileMime

getBookRawFileR :: Int -> Text -> Handler TypedContent
getBookRawFileR id zipFilePath = do
    abType <- getBook id 
        >>= getAudiobookType
        >>= either (invalidArgs . (:[]) . T.pack . show) return
    case abType of
        SingleFile _ path ->
            sendFileMime path
        Zip _ zipPath filePaths -> do
            let mime = defaultMimeLookup zipFilePath
            conduit <- liftIO $ getSingleFile zipPath (T.unpack zipFilePath)
            respondSource mime $ mapOutput (Chunk . BSB.fromByteString) conduit


getBookOverlayR :: Int -> Handler Html
getBookOverlayR id = do
    book <- getBook id
    defaultLayout [whamlet|
        <div .modal-content>
            <div .modal-header>
                <button .close type="button" data-dismiss="modal" aria-label="Close">
                    <span aria-hidden="true">&times;
            <div .modal-body.row>
                <div .col-md-3>
                    <img style="height: 250px" src=@{BookCoverR (bookId $ fst book)}>
                <div .col-md-9>
                    <h4> #{ bookTitle $ fst book }
                    <button type="button" .btn.btn-primary> Copy RSS link
                    <button type="button" .btn.btn-primary> Download MP3
    |]
