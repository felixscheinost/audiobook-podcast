{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Home where

import Foundation
import Yesod.Core
import Database.Calibre

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    let books = [] :: [Audiobook]
    [whamlet|
        <div class="container">
            <div class="row">
                <input type="text" placeholder="Search" id="audiobook-search" class="col-lg-4">
            <div class="row" id="audiobook-container">
                $forall book <- books
                    <div class="col-lg-2">
                        <div>
                            <img style="height: 250px" src="#{ abCover book }">
                        <span class="font-weight-bold">#{ abTitle book }
    |]
