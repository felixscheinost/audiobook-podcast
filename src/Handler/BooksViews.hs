{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.BooksViews where

import           Database   (Audiobook, AudiobookT (..))
import qualified Database
import           Foundation
import           Import


searchWidget :: Maybe Widget -> Widget
searchWidget additionalTools =
    [whamlet|
        <div .row #toolbar>
            <div .col-md-4 #search>
                <input type="text" .form-control placeholder=_{MsgSearch}>
            <div .col-md-8>
                $maybe w <- additionalTools
                    ^{w}
    |]

audiobookContainerWidget :: [Audiobook] -> Widget
audiobookContainerWidget books =
    [whamlet|
        $forall Audiobook{abId=abId} <- books
            <div .audiobook .col-md-2>
                <div .img-wrapper>
                    <img src=@{BookCoverR abId} data-modal-url=@{BookOverlayR abId}>
    |]

getBookViewR :: Handler Html
getBookViewR = do
    runSQL Database.listBooks >>= \books ->
        defaultLayout [whamlet|
            ^{searchWidget Nothing}
            <div .row #audiobook-container>
                ^{audiobookContainerWidget books}
    |]


postBookViewR :: Handler Html
postBookViewR = do
    query <- lookupPostParam "query"
    books <- runSQL (Database.listBooksQuery query)
    pc <- widgetToPageContent $ audiobookContainerWidget books
    withUrlRenderer [hamlet|
        ^{pageBody pc}
    |]

getHomeR :: Handler Html
getHomeR = redirect BookViewR
