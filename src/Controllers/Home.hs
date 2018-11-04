{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Controllers.Home where

import qualified Data.Text        as T
import qualified Data.Text.Read   as TR
import           Database.Calibre
import           Foundation
import           Yesod.Core
import Audiobook (Audiobook(..))
import qualified Audiobook as AB

showAudioBooks :: [Audiobook] -> Handler Widget
showAudioBooks books =
    return [whamlet|
        <div .row #audiobook-container>
            $forall Audiobook{abId=abId} <- books
                <div .audiobook .ajax-modal data-modal-url=@{BookOverlayR abId}>
                    <div .img-wrapper>
                        <img style="height: 250px" src=@{BookCoverR abId}>
    |]

booksView :: Handler [Audiobook] -> Handler Html
booksView books = do
    booksW <- books >>= showAudioBooks
    defaultLayout [whamlet|
        <div .container>
            <div .row .d-flex #toolbar>
                <div .col-md-4 #search>
                    <input type="text" .form-control placeholder=_{MsgSearch}>
                <div .col-md-8>
                    <button .float-left.btn.btn-primary #select>_{MsgSelectBooks}
            ^{booksW}
    |]


getBookViewR :: Handler Html
getBookViewR = booksView AB.listAudiobooks

getSeriesViewR :: Handler Html
getSeriesViewR = do
    seriesWithBookIds <- AB.listSeries
    defaultLayout [whamlet|
        <div .container>
            <div .row .d-flex #toolbar>
                <div .col-md-4 #search>
                    <input type="text" .form-control placeholder=_{MsgSearch}>
            <div .row #series-container>
                $forall (series, bookIds) <- seriesWithBookIds
                    <a href=@{SingleSeriesViewR (seriesId series)} .series .text-dark .text-center .font-weight-bold>
                        $case take 1 $ map TR.decimal $ T.splitOn "," bookIds
                            $of [(Right (bookId, _))]
                                <img .img src=@{BookCoverR bookId}>
                            $of _
                                <div .img style="height: 250px; width: 158px">
                        #{seriesName series}

    |]

getSingleSeriesViewR :: Int -> Handler Html
getSingleSeriesViewR _seriesId = booksView (AB.listAudiobooksInSeries _seriesId)
    
getHomeR :: Handler Html
getHomeR = redirect BookViewR
