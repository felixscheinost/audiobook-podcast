{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Home where

import Foundation
import Yesod.Core
import Database.Calibre

getHomeR :: Handler Html
getHomeR = do
    books <- runSQL getAllAudiobooks
    defaultLayout [whamlet|
        <input type="text" placeholder="Search" #audiobook-search>
        <div .row #audiobook-container>
            $forall (book, _) <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                    <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]
