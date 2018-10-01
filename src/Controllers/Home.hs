{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Home where

import Foundation
import Yesod.Core
import Database.Calibre.Queries
import Database.Calibre.Tables
import Control.Concurrent.MVar (takeMVar, putMVar)
import Database.SQLite.Simple (Connection)

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    app <- getYesod
    books <- liftIO $ do
        conn <- takeMVar (appDbConnection app)
        books <- getAllAudiobooks conn
        putMVar (appDbConnection app) conn
        return books
    [whamlet|
        <div class="container">
            <div class="row">
                <input type="text" placeholder="Search" id="audiobook-search" class="col-lg-4">
            <div class="row" id="audiobook-container">
                $forall (book, bookData) <- books
                    <div class="col-lg-2">
                        <div>
                            <img style="height: 250px" src="https://via.placeholder.com/170x250">
                        <span class="font-weight-bold">#{ bookTitle book }
    |]
