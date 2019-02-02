{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.BooksViews where

import           Database         (Audiobook, AudiobookT (..))
import qualified Database
import           Foundation
import           Import
import qualified Library
import qualified System.Directory as Directory


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

singleBook :: Audiobook -> Widget
singleBook book@Audiobook{abAuthor=abAuthor, abTitle=abTitle} = do
    coverExists <- liftIO $ Directory.doesFileExist $ Library.getAudiobookCover book
    [whamlet|
        <div .audiobook .col-md-3 .col-lg-2 .col-xl-2 .col-sm-3 .col-4>
            $if coverExists
                <div .img-wrapper>
                    <img src=@{BookCoverByAuthorTitleR abAuthor abTitle}
                        data-modal-url=@{BookOverlayByAuthorTitleR abAuthor abTitle}>
            $else
                <div .img-wrapper>
                    <img src=@{StaticR img_cover_placeholder_svg}
                        data-modal-url=@{BookOverlayByAuthorTitleR abAuthor abTitle}>
                    <span .placeholder-title>
                        #{abTitle}
                        <br>
                        <small>#{abAuthor}
    |]


audiobookContainerWidget :: [Audiobook] -> Widget
audiobookContainerWidget books =
    [whamlet|
        $forall book <- books
            ^{singleBook book}
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
