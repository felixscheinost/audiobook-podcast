{-# LANGUAGE QuasiQuotes #-}

module Handler.BooksViews where

import qualified Data.Char  as C
import qualified Data.Text  as T
import           Database   (AbAuthor, AbSeries, AbTitle, AudiobookT (abTitle))
import qualified Database
import           Foundation
import           Import
import           Library    (SeriesCover (..))
import qualified Library

searchWidget :: Maybe Text -> Maybe Widget -> Widget
searchWidget query additionalTools =
    [whamlet|
        <div .row #toolbar>
            <div .col-md-4 #search>
                <form method=get .form-inline>
                    <div .input-group>
                        <input type="text" .form-control placeholder=_{MsgSearch} name=query value=#{fromMaybe "" query} autocomplete=off>
                        <div .input-group-append>
                            <button type="submit" .btn.btn-primary>
                                <span .fa.fa-search>
            <div .col-md-8>
                $maybe w <- additionalTools
                    ^{w}
    |]

singleBook :: Maybe Text -> (AbAuthor, Maybe AbSeries, Maybe AbTitle) -> Widget
singleBook searchQuery (author, series, title) = do
    seriesWithBooks <- flip (maybe (return Nothing)) series $ \abSeries -> do
        books <- runSQL (Database.getAudiobooksByAuthorSeries author abSeries)
        cover <- Library.getSeriesCover books
        return $ Just (abSeries, cover)
    urlRender <- getUrlRenderParams
    let modalUrl = case (series, title) of
            (Just abSeries, _) -> urlRender (SeriesOverlayR author abSeries) []
            (_, Just abTitle) -> urlRender (BookOverlayR author abTitle) []
            _ -> ""
    let titleOrSeries = case (series, title) of
            (Just abSeries, _) -> toHtml abSeries
            (_, Just abTitle)  -> toHtml abTitle
            _                  -> ""
    let bookTitleLinkHref = urlRender BookViewR $ ("modalUrl", modalUrl) : maybeToList (("query", ) <$> searchQuery)
    [whamlet|
        <div .audiobook .col-4 .col-sm-3 .col-md-3 .col-lg-2 .col-xl-2>
            <div .audiobook-wrapper data-modal-url=#{modalUrl}>
                $case (seriesWithBooks, title)
                    $of (Just (_, GeneratedGrid audiobooksAndPaths), _)
                        <div .img-wrapper.four>
                            $forall (book, _) <- audiobooksAndPaths
                                <img src=@{BookCoverR author (abTitle book)}>
                    $of (Just (abSeries, _), _)
                        <div .img-wrapper.one>
                            <img src=@{SeriesCoverR author abSeries}>
                    $of (_, Just abTitle)
                        <div .img-wrapper.one>
                            <img src=@{BookCoverR author abTitle}>
                    $of _
                        <div .img-wrapper.one>
                            <img src=@{StaticR img_cover_placeholder_svg}>
                <div .text-wrapper>
                    <a href=#{bookTitleLinkHref} .text-bold rel=title> #{titleOrSeries}
                    <br>
                    <span .text-small> #{author}
    |]

audiobookContainerWidget :: Maybe Text -> [(AbAuthor, Maybe AbSeries, Maybe AbTitle)] -> Widget
audiobookContainerWidget searchQuery books =
    [whamlet|
        $forall book <- books
            ^{singleBook searchQuery book}
    |]

getBookViewR :: Handler Html
getBookViewR = do
    query <- lookupGetParam "query"
    books <- runSQL (Database.listBooksQuery query)
    defaultLayout $ do
        case query of
            Just q ->
                if isJust $ T.find (not . C.isSpace) q then
                    setTitle $ toHtml $ "Audiobook-Podcast: " ++ q
                else
                    setTitle $ toHtml ("Audiobook-Podcast" :: Text)
            Nothing -> setTitle $ toHtml ("Audiobook-Podcast" :: Text)
        [whamlet|
            ^{searchWidget query Nothing}
            <div .row #audiobook-container>
                ^{audiobookContainerWidget query books}
        |]
