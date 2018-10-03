{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Book where

import Import
import Database.Calibre
import Network.Mime (defaultMimeLookup)
import System.FilePath (takeFileName)
import Data.Text as T

getBookCoverR :: Int -> Handler Html
getBookCoverR id = do
    bookQueryResult <- runSQL (getAudiobook id)
    (flip $ maybe notFound) bookQueryResult $ \book -> do
        coverPath <- abCover book
        let mime = defaultMimeLookup $ T.pack $ takeFileName coverPath
        sendFile mime coverPath

getBookOverlayR :: Int -> Handler Html
getBookOverlayR id = do
    bookQueryResult <- runSQL (getAudiobook id)
    (flip $ maybe notFound) bookQueryResult $ \book -> defaultLayout
        [whamlet|
            <div .modal-content>
                <div .modal-header>
                    <button .close type="button" data-dismiss="modal" aria-label="Close">
                        <span aria-hidden="true">&times;
                <div .modal-body.row>
                    <div .col-md-3>
                        <img style="height: 250px" src=@{BookCoverR (bookId $ fst book)}>
                    <div .col-md-9>
                        <h4> #{ abTitle book }
                        <button type="button" .btn.btn-primary> Copy RSS link
                        <button type="button" .btn.btn-primary> Download MP3

        |]