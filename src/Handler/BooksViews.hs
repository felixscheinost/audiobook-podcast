{-# LANGUAGE QuasiQuotes #-}

module Handler.BooksViews where

import qualified Data.Char        as C
import qualified Data.Text        as T
import           Database         (AbAuthor, AbSeries, AbTitle,
                                   AudiobookT (abTitle))
import qualified Database
import           Foundation
import           Import
import           Library          (SeriesCover (..))
import qualified Library
import           Yesod.Core.Types (ErrorResponse (InternalError),
                                   HandlerContents (HCError))

searchWidget :: Maybe Text -> Widget
searchWidget query =
    [whamlet|
        <div .row #toolbar>
            <div .col-md-4 #search>
                <form method=get .form-inline>
                    <div .input-group>
                        <input type="text" .form-control placeholder=_{MsgSearch} name=query value=#{fromMaybe "" query} autocomplete=off>
                        <div .input-group-append>
                            <button type="submit" .btn.btn-primary>
                                <span .fa.fa-search>
    |]

data BookOrSeries = Book AbTitle | Series AbSeries SeriesCover

singleBook :: Maybe Text -> (AbAuthor, Maybe AbSeries, Maybe AbTitle) -> Widget
singleBook searchQuery (author, series, title) = do
    bookOrSeries <- case (series, title) of
            (Just abSeries, _) -> do
                books <- runSQL (Database.getAudiobooksByAuthorSeries author abSeries)
                seriesCover <- Library.getSeriesCover books
                return (Series abSeries seriesCover)
            (_, Just abTitle) ->
                return (Book abTitle)
            (Nothing, Nothing) ->
                liftIO $ throwIO $ HCError $ InternalError "Shouldn't happen: Result was neither series or book"
    urlRender <- getUrlRenderParams
    let currentUrlWithModalUrl modalRoute = urlRender BookViewR $ ("modalUrl", urlRender modalRoute []) : maybeToList (("query", ) <$> searchQuery)
    [whamlet|
        <div .audiobook .col-4 .col-sm-3 .col-md-3 .col-lg-2 .col-xl-2>
            $case bookOrSeries
                $of Series abSeries cover
                    <div .audiobook-wrapper data-modal-url=@{SeriesViewR author abSeries}>
                        $case cover
                            $of GeneratedGrid audiobooksAndPaths
                                <a .img-wrapper.four>
                                    $forall (book, _) <- audiobooksAndPaths
                                        <img src=@{BookCoverR author (abTitle book)}>
                            $of _
                                <a .img-wrapper.one>
                                    <div .img-wrapper.one>
                                        <img src=@{SeriesCoverR author abSeries}>
                        <div .text-wrapper>
                            <a href=#{currentUrlWithModalUrl (SeriesViewR author abSeries)} .text-bold> #{abSeries}
                            <br>
                            <span .text-small> #{author}
                $of Book abTitle
                    <div .audiobook-wrapper data-modal-url=@{BookOverlayR author abTitle}>
                        <div .img-wrapper.one>
                            <img src=@{BookCoverR author abTitle }>
                        <div .text-wrapper>
                            <a href=#{currentUrlWithModalUrl (BookOverlayR author abTitle)} .text-bold rel=title> #{abTitle}
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
                    setTitle $ toHtml $ "audiobook podcast - search - " ++ q
                else
                    setTitle $ toHtml ("audiobook podcast" :: Text)
            Nothing -> setTitle $ toHtml ("audiobook podcast" :: Text)
        [whamlet|
            ^{searchWidget query}
            <div .row #audiobook-container>
                ^{audiobookContainerWidget query books}
        |]
