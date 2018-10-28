{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Home where

import Foundation
import Yesod.Core
import Database.Calibre
import qualified Data.Text as T
import qualified Data.Text.Read as TR

msgForView :: Route App -> AppMessage
msgForView BookViewR = MsgBookView
msgForView AuthorViewR = MsgAuthorView
msgForView SeriesViewR = MsgSeriesView
msgForView (SingleSeriesViewR _) = MsgSeriesView
msgForView _ = MsgBookView

views :: [Route App]
views = 
    [ BookViewR 
    , AuthorViewR 
    , SeriesViewR
    ]

showSearchBar :: Handler Widget
showSearchBar = do
    currentRoute <- getCurrentRoute
    return [whamlet|
        <div .input-group.w-100 #search-bar>
            <div .input-group-prepend>
                <button .btn.btn-outline-secondary.dropdown-toggle type=button data-toggle=dropdown aria-haspopup=true aria-expanded=false>
                    _{maybe MsgBookView msgForView currentRoute}
                <div .dropdown-menu>
                    $forall route <- views
                        <a .dropdown-item :Just route == currentRoute:.active href=@{route}> _{msgForView route}
            <input type="text" .form-control placeholder=_{MsgSearch}>
    |]

showBooks :: [BookAndData] -> Handler Widget
showBooks books = 
    return [whamlet|
        <div .row #audiobook-container>
            $forall (book, _) <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                    <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]
    

getBookViewR :: Handler Html
getBookViewR = do
    searchBar <- showSearchBar
    books <- runSQL getAllAudiobooks >>= showBooks
    defaultLayout [whamlet|
        ^{searchBar} 
        ^{books} 
    |]

getAuthorViewR :: Handler Html
getAuthorViewR = do
    searchBar <- showSearchBar
    books <- runSQL getAllAudiobooks
    defaultLayout [whamlet|
        ^{searchBar} 
        <div .row #audiobook-container>
            $forall (book, _) <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                    <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]
    

getSeriesViewR :: Handler Html
getSeriesViewR = do
    searchBar <- showSearchBar
    seriesWithBookIds <- runSQL getAllSeries
    defaultLayout [whamlet|
        ^{searchBar} 
        <div .row #series-container>
            $forall (series, bookIds) <- seriesWithBookIds
                <a href=@{SingleSeriesViewR (seriesId series)} .series .text-dark .text-center .font-weight-bold>
                    $case take 1 $ map TR.decimal $ T.splitOn "," bookIds
                        $of [(Right (bookId, _))]
                            <img .img src=@{BookCoverR bookId}>
                        $of _
                            <div .img style="height: 250px; width: 158px">
                    #{seriesName series}
                    
    |]

getSingleSeriesViewR :: Int -> Handler Html
getSingleSeriesViewR _seriesId = do
    searchBar <- showSearchBar
    books <- runSQL (getAllAudiobooksInSeries _seriesId) >>= showBooks
    defaultLayout [whamlet|
        ^{searchBar} 
        ^{books} 
    |]

getHomeR :: Handler Html
getHomeR = redirect BookViewR
