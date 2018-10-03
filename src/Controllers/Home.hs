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
        <div .container>
            <div .row #search-container>
                <div .col-lg-4.col-12 >
                    <input type="text" placeholder="Search" #audiobook-search>
            <div .row #audiobook-container>
                $forall (book, bookData) <- books
                    <div .col-xl-2 .col-lg-3 .col-md-4 .col-sm-5 .col-6 .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                        <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]
