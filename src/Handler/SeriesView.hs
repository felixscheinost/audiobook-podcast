{-# LANGUAGE QuasiQuotes #-}

module Handler.SeriesView where

import qualified Data.Char          as C
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text          as T
import           Database           (AbAuthor, AbSeries, AbTitle, Audiobook,
                                     AudiobookT (..))
import qualified Database
import           Foundation
import           Import
import           Library            (SeriesCover (..))
import qualified Library

getSeriesViewR :: AbAuthor -> AbSeries -> Handler Html
getSeriesViewR abAuthor abSeries = do
    maybeBooks <- runSQL (Database.getAudiobooksByAuthorSeries abAuthor abSeries)
    books <- maybe notFound return $ NEL.nonEmpty maybeBooks
    pc <- widgetToPageContent [whamlet|
        <div .modal-header>
            <h5 .modal-title> #{abSeries}
            <button .close type="button" data-dismiss="modal" aria-label="Close">
                <span aria-hidden="true">&times;
        <div .modal-body .book-modal>
            <div .input-group>
                <input type="text" .form-control readonly="" value=@{BookViewR} #copy-rss-input>
                <div .input-group-append>
                    <button .btn.btn-secondary type="button" #copy-rss-button data-clipboard-target="#copy-rss-input">
                        _{MsgCopyRSSLink}
            <ul .list-unstyled>
                $forall Audiobook{abTitle} <- books
                    <li .media.mb-2>
                        <img .mr-3 width="94px" src=@{BookCoverR abAuthor abTitle}>
                        <div .media-body>
                            <h6 .mt-0.mb-1>
                                #{abTitle}
    |]
    withUrlRenderer [hamlet|
        ^{pageBody pc}
    |]
