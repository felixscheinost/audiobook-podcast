{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

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
        $forall Audiobook{abAuthor=abAuthor, abTitle=abTitle} <- books
            <div .audiobook .col-md-3 .col-lg-2 .col-xl-2 .col-sm-3 .col-4>
                <div .img-wrapper>
                    <img src=@{BookCoverByAuthorTitleR abAuthor abTitle} data-modal-url=@{BookOverlayByAuthorTitleR abAuthor abTitle}>
    |]

getBookViewR :: Handler Html
getBookViewR =
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
