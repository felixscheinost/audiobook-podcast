{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Home where

import Foundation
import Yesod.Core
import Database.Calibre
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

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
    searchBar <- searchBarWidget
    books <- runSQL getAllAudiobooks
    defaultLayout [whamlet|
        ^{searchBar} 
        <div .row #audiobook-container>
            $forall (book, _) <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR (bookId book)}>
                    <img style="height: 250px" src=@{BookCoverR (bookId book)}>
    |]

getAuthorViewR :: Handler Html
getAuthorViewR = do
    searchBar <- searchBarWidget
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
    searchBar <- searchBarWidget
    seriesWithBookIds <- runSQL getAllSeries
    defaultLayout [whamlet|
        ^{searchBar} 
        <div .row #series-container>
            $forall (series, bookIds) <- seriesWithBookIds
                <a href="#" .series .text-dark .text-center .font-weight-bold>
                    $case take 1 $ map TR.decimal $ T.splitOn "," bookIds
                        $of [(Right (bookId, _))]
                            <img .img src=@{BookCoverR bookId}>
                        $of _
                            <div .img style="height: 250px; width: 158px">
                    #{seriesName series}
                    
    |]



getHomeR :: Handler Html
getHomeR = redirect BookViewR
