{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Home where

import Foundation
import Yesod.Core
import Database.Calibre
import Database.SQLite.Simple (Connection)
import Import

getHomeR :: Handler Html
getHomeR = do
    books <- runSQL getAllAudiobooks
    defaultLayout [whamlet|
        <div class="container">
            <div class="row">
                <input type="text" placeholder="Search" id="audiobook-search" class="col-lg-4">
            <div class="row" id="audiobook-container">
                $forall (book, bookData) <- books
                    <div class="col-xl-2 col-lg-3 col-md-3 col-sm-4 col-6 audiobook ajax-modal" data-modal-url=@{BookOverlayR (bookId book)}>
                        <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]
