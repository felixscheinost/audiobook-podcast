{-# LANGUAGE QuasiQuotes #-}

module Handler.SeriesView where

import qualified Data.Char          as C
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text          as T
import           Database           (AbAuthor, AbSeries, AbTitle,
                                     AudiobookT (abTitle))
import qualified Database
import           Foundation
import           Import
import           Library            (SeriesCover (..))
import qualified Library

getSeriesViewR :: AbAuthor -> AbSeries -> Handler Html
getSeriesViewR abAuthor abSeries = do
    maybeBooks <- runSQL (Database.getAudiobooksByAuthorSeries abAuthor abSeries)
    books <- maybe notFound return $ NEL.nonEmpty maybeBooks
    defaultLayout $ do
        setTitle $ toHtml ("audiobook podcast - " :: Text) ++ toHtml abSeries
        [whamlet|
            <div .row #toolbar>
                <button href="#" .btn.btn-primary.mr-4 onclick="window.history.back()">
                    <span .fa.fa-chevron-left>
                    Back
                <div .align-bottom>
                    <h2.d-inline.mr-2>
                        #{abSeries}
                    <h6.d-inline.text-muted>
                        #{abAuthor}
        |]
