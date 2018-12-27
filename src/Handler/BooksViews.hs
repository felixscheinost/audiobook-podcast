{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.BooksViews where

import           Database   (Audiobook, AudiobookT (..))
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

booksView :: Handler [Audiobook] -> Handler Html
booksView getBooks = getBooks >>= \books ->
    defaultLayout [whamlet|
        <div .container>
            ^{searchWidget Nothing}
            <div .row #audiobook-container>
                $forall Audiobook{abId=abId} <- books
                    <div .audiobook .col-md-2>
                        <div .img-wrapper>
                            <img src=@{BookCoverR abId} data-modal-url=@{BookOverlayR abId}>
    |]


getBookViewR :: Handler Html
getBookViewR = booksView (return [])

getHomeR :: Handler Html
getHomeR = redirect BookViewR
