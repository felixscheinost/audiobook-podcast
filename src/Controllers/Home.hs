{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Home where

import Foundation
import Yesod.Core
import Database.Calibre
import Data.List (find)

data View = View (Route App) AppMessage

instance Eq View where
    (View r1 _) == (View r2 _) = r1 == r2

views :: [View]
views = 
    [ View BookViewR MsgBookView
    , View AuthorViewR MsgAuthorView
    , View SeriesViewR MsgSeriesView
    ]

viewForRoute :: Route App -> Maybe View
viewForRoute r = find (\(View r' _) -> r == r') views

getSelectedView :: Handler (Maybe View)
getSelectedView = (>>= viewForRoute) <$> getCurrentRoute

searchBarWidget :: Handler Widget
searchBarWidget = do
    selectedView <- getSelectedView
    return [whamlet|
        <div .input-group.w-100 #search-bar>
            <div .input-group-prepend>
                $maybe (View _ msg) <- selectedView
                    <button .btn.btn-outline-secondary.dropdown-toggle type=button data-toggle=dropdown aria-haspopup=true aria-expanded=false>
                        _{msg}
                <div .dropdown-menu>
                    $forall view@(View route msg) <- views
                        <a .dropdown-item :Just view == selectedView:.active href=@{route}> _{msg}
            <input type="text" .form-control placeholder=_{MsgSearch}>
    |]
    

getBookViewR :: Handler Html
getBookViewR = do
    books <- runSQL getAllAudiobooks
    searchBar <- searchBarWidget
    defaultLayout [whamlet|
        ^{searchBar} 
        <div .row #audiobook-container>
            $forall (book, _) <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                    <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]

getAuthorViewR :: Handler Html
getAuthorViewR = do
    books <- runSQL getAllAudiobooks
    searchBar <- searchBarWidget
    defaultLayout [whamlet|
        ^{searchBar} 
        <div .row #audiobook-container>
            $forall (book, _) <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                    <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]
    

getSeriesViewR :: Handler Html
getSeriesViewR = do
    books <- runSQL getAllAudiobooks
    searchBar <- searchBarWidget
    defaultLayout [whamlet|
        ^{searchBar} 
        <div .row #audiobook-container>
            $forall (book, _) <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                    <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]



getHomeR :: Handler Html
getHomeR = redirect BookViewR
