{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.BooksViews where

import           Database         (AbAuthor, AbSeries, AbTitle, Audiobook,
                                   AudiobookT (..))
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

singleBook :: (AbAuthor, Maybe AbSeries, Maybe AbTitle) -> Widget
singleBook (author, series, title) =
    [whamlet|
        <div .audiobook .col-4 .col-sm-3 .col-md-3 .col-lg-2 .col-xl-2>
            <div .audiobook-wrapper>
                $case (series, title)
                    $of (Just abSeries, _)
                        <div .img-wrapper>
                            <img src=@{SeriesCoverR author abSeries}
                                data-modal-url=@{SeriesOverlayR author abSeries}>
                        <div .text-wrapper>
                            <span .text-bold> #{abSeries}
                            <br>
                            <span .text-small> #{author}
                    $of (_, Just abTitle)
                        <div .img-wrapper>
                            <img src=@{BookCoverR author abTitle}
                                data-modal-url=@{BookOverlayR author abTitle}>
                        <div .text-wrapper >
                            <span .text-bold> #{abTitle}
                            <br>
                            <span .text-small> #{author}
                    $of _
                        <br>
    |]

audiobookContainerWidget :: [(AbAuthor, Maybe AbSeries, Maybe AbTitle)] -> Widget
audiobookContainerWidget books =
    [whamlet|
        $forall book <- books
            ^{singleBook book}
    |]

getBookViewR :: Handler Html
getBookViewR =
    runSQL (Database.listBooksQuery Nothing) >>= \books ->
        defaultLayout [whamlet|
            ^{searchWidget Nothing}
            <div .row #audiobook-container>
                ^{audiobookContainerWidget books}
    |]


postBookViewR :: Handler Html
postBookViewR = do
    query <- lookupPostParam "query"
    booksAndSeries <- runSQL (Database.listBooksQuery query)
    pc <- widgetToPageContent $ audiobookContainerWidget booksAndSeries
    withUrlRenderer [hamlet|
        ^{pageBody pc}
    |]

getHomeR :: Handler Html
getHomeR = redirect BookViewR
