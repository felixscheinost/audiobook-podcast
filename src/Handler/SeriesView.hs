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
        <div .modal.fade tabindex="-1" role="dialog" aria-hidden="true">
            <div .modal-dialog>
                <div .modal-content>
                    <div .modal-header>
                        <h5 .modal-title> Bücher auswählen
                        <button .close type="button" data-dismiss="modal" aria-label="Close">
                            <span aria-hidden="true">&times;
                    <div .modal-body .book-modal>
                        <div .d-flex.justify-content-between.mb-3>
                            <div .align-self-center>
                                <p .text-muted.m-0>Bitte mindestens ein Buch auswählen
                            <button .btn.btn-primary.disabled>
                                        OK
                        <table .table.table-striped data-rel="series-book-selection">
                            $forall Audiobook{abTitle} <- books
                                <tr .cursor-pointer>
                                    <td>
                                        <input type="checkbox">
                                    <td>
                                        <img .mr-3 width="64px" src=@{BookCoverR abAuthor abTitle}>
                                    <td>
                                        <h6 .mt-0.mb-1>
                                            #{abTitle}
    |]
    withUrlRenderer [hamlet|
        ^{pageBody pc}
    |]
