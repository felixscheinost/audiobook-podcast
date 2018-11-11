{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.BooksViews where

import qualified Data.Aeson                 as JSON
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Read             as TR
import           Database.Calibre
import           Database.Calibre.Audiobook (Audiobook (..))
import qualified Database.Calibre.Audiobook as AB
import           Foundation
import           Import


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

selectBooksButton :: Widget
selectBooksButton = [whamlet|
    <button .float-left.btn.btn-primary #select data-text-inactive=_{MsgSelectBooks} data-text-active=_{MsgCancel}>
        _{MsgSelectBooks}
|]

booksView :: Handler [Audiobook] -> Handler Html
booksView getBooks = getBooks >>= \books ->
    defaultLayout [whamlet|
        <div .container>
            ^{searchWidget Nothing}
            <div .row #audiobook-container>
                $forall Audiobook{abId=abId} <- books
                    <div .audiobook .col-md-2>
                        <div .img-wrapper>
                            <img src=@{BookCoverR abId} data-modal-url=@{BookOverlayR abId}>
    |]


getBookViewR :: Handler Html
getBookViewR = booksView AB.listAudiobooks

getSingleSeriesViewR :: Int -> Handler Html
getSingleSeriesViewR _seriesId = booksView (AB.listAudiobooksInSeries _seriesId)

convertBooksButtons :: Widget
convertBooksButtons = [whamlet|
    ^{selectBooksButton}
    <form method="post">
        <input type="hidden" #convertIds name="convertIds"/>
        <button type="submit" .float-left.btn.btn-primary.disabled #convert>
            _{MsgConvert}

|]

getBookNeedConvertR :: Handler Html
getBookNeedConvertR = do
    books <- AB.listAudiobooksNeedConversion
    defaultLayout [whamlet|
        <div .container>
            ^{searchWidget (Just convertBooksButtons) }
            <div .row #audiobook-container>
                $forall Audiobook{abId=abId} <- books
                    <div .col-xl-2.col-lg-3.col-md-4.col-6 .audiobook data-book-id=#{abId}>
                        <img src=@{BookCoverR abId}>
    |]

postBookNeedConvertR :: Handler Html
postBookNeedConvertR = do
    idsStr <- TE.encodeUtf8 . fromMaybe "" <$> lookupPostParam "convertIds"
    let ids = JSON.decodeStrict idsStr :: Maybe [Int]
    liftIO $ print ids
    redirect BookNeedConvertR

getSeriesViewR :: Handler Html
getSeriesViewR = do
    seriesWithBookIds <- AB.listSeries
    defaultLayout [whamlet|
        <div .container>
            ^{searchWidget Nothing}
            <div .row #series-container>
                $forall (series, bookIds) <- seriesWithBookIds
                    <a href=@{SingleSeriesViewR (seriesId series)} .series .text-dark .text-center .font-weight-bold>
                        $case take 1 $ map TR.decimal $ T.splitOn "," bookIds
                            $of [(Right (bookId, _))]
                                <img .img src=@{BookCoverR bookId}>
                            $of _
                                <div .img>
                        #{seriesName series}

    |]

getHomeR :: Handler Html
getHomeR = redirect BookViewR
