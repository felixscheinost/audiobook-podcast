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

imageUrl :: AbAuthor -> Maybe AbSeries -> Maybe AbTitle -> Route App
imageUrl abAuthor (Just abSeries) Nothing = SeriesCoverR abAuthor abSeries
imageUrl abAuthor Nothing (Just abTitle)  = BookCoverR abAuthor abTitle

modalUrl :: AbAuthor -> Maybe AbSeries -> Maybe AbTitle -> Route App
modalUrl abAuthor (Just abSeries) Nothing = SeriesOverlayR abAuthor abSeries
modalUrl abAuthor Nothing (Just abTitle)  = BookOverlayR abAuthor abTitle

singleBook :: (AbAuthor, Maybe AbSeries, Maybe AbTitle) -> Widget
singleBook (author, series, title) =
    [whamlet|
        <div .audiobook .col-md-3 .col-lg-2 .col-xl-2 .col-sm-3 .col-4>
                <div .img-wrapper>
                    <img src=@{imageUrl author series title}
                         data-modal-url=@{modalUrl author series title}>
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
