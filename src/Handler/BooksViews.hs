{-# LANGUAGE QuasiQuotes #-}

module Handler.BooksViews where

import           Database   (AbAuthor, AbSeries, AbTitle, AudiobookT (abTitle))
import qualified Database
import           Foundation
import           Import
import           Library    (SeriesCover (..))
import qualified Library

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
singleBook (author, series, title) = do
    seriesWithBooks <- flip (maybe (return Nothing)) series $ \abSeries -> do
        books <- runSQL (Database.getAudiobooksByAuthorSeries author abSeries)
        cover <- Library.getSeriesCover books
        return $ Just (abSeries, cover)
    [whamlet|
        <div .audiobook .col-4 .col-sm-3 .col-md-3 .col-lg-2 .col-xl-2>
            <div .audiobook-wrapper>
                $case (seriesWithBooks, title)
                    $of (Just (abSeries, cover), _)
                        $case cover
                            $of GeneratedGrid audiobooksAndPaths
                                <div .img-wrapper.four data-modal-url=@{SeriesOverlayR author abSeries}>
                                    $forall (book, _) <- audiobooksAndPaths
                                        <img src=@{BookCoverR author (abTitle book)}>
                            $of _
                                <div .img-wrapper.one data-modal-url=@{SeriesOverlayR author abSeries}>
                                    <img src=@{SeriesCoverR author abSeries}>

                        <div .text-wrapper>
                            <span .text-bold> #{abSeries}
                            <br>
                            <span .text-small> #{author}
                    $of (_, Just abTitle)
                        <div .img-wrapper.one data-modal-url=@{BookOverlayR author abTitle}>
                            <img src=@{BookCoverR author abTitle}>
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
getBookViewR = do
    query <- lookupGetParam "query"
    runSQL (Database.listBooksQuery query) >>= \books ->
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
