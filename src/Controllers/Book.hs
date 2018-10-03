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
import Prelude ((!!))
import qualified Codec.Archive.Zip as Z

getBook :: Int -> Handler BookAndData
getBook id = do
    bookQueryResult <- runSQL (getAudiobook id)
    case bookQueryResult of
        Just book -> return book
        _         -> notFound

sendFileMime :: FilePath -> Handler TypedContent
sendFileMime fp = do
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    sendFile mime fp

getBookCoverR :: Int -> Handler TypedContent
getBookCoverR id = do
    getBook id >>= bookCover >>= sendFileMime

getBookRawFileR :: Int -> Int -> Handler TypedContent
getBookRawFileR id fileIndex = do
    abTypeErr <- getBook id >>= getAudiobookType
    case abTypeErr of
        Left _ -> invalidArgs ["Book has no valid audio files"]
        Right abType -> case abType of
            SingleFile _ path ->
                sendFileMime path
            Zip _ zipPath filePaths ->
                if fileIndex < 0 || fileIndex >= length filePaths then
                    invalidArgs ["No audio file with that index"]
                else do
                    let fp = filePaths !! fileIndex
                    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
                    conduit <- liftIO $ getSingleFile zipPath fp
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
