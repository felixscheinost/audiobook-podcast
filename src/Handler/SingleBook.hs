{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.SingleBook where

import qualified Data.Text        as T
import           Data.Time.Clock  (getCurrentTime)
import           Database         (AbAuthor, AbSeries, AbTitle (..), Audiobook,
                                   AudiobookT (..))
import qualified Database         as DB
import qualified Handler.SendFile as SendFile
import           Import           hiding (count, fileSize)
import qualified Library
import qualified System.Directory as Directory
import           Yesod.RssFeed    (RepRss, rssFeed)

withBook :: (Audiobook -> Handler a) -> AbAuthor -> AbTitle -> Handler a
withBook f _author _title = runSQLGetOr404 (DB.getAudiobookByAuthorTitle _author _title) >>= f

withSeriesBooks :: ([Audiobook] -> Handler a) -> AbAuthor -> AbSeries -> Handler a
withSeriesBooks f _author _series = runSQL (DB.getAudiobooksByAuthorSeries _author _series) >>= f

getBookCoverR :: AbAuthor -> AbTitle -> Handler TypedContent
getBookCoverR = withBook $ \book -> do
    let path = Library.getAudiobookCover book
    exists <- liftIO $ Directory.doesFileExist path
    if exists then
        SendFile.sendFileMime path
    else
        redirect (StaticR img_cover_placeholder_svg)

getSeriesCoverR :: AbAuthor -> AbSeries -> Handler TypedContent
getSeriesCoverR = withSeriesBooks $ \case
        []     -> notFound
        book:_ -> SendFile.sendFileMime (Library.getAudiobookCover book)

getBookFileR :: AbAuthor -> AbTitle -> Handler TypedContent
getBookFileR = withBook (SendFile.sendFileMime . T.unpack . abPath)

bookOverlay :: Audiobook -> Handler Html
bookOverlay Audiobook{abTitle, abAuthor} = do
    -- TODO: Probably don't need a widget here
    pc <- widgetToPageContent [whamlet|
        <div .modal-header>
            <h5 .modal-title> #{abTitle}
            <button .close type="button" data-dismiss="modal" aria-label="Close">
                <span aria-hidden="true">&times;
        <div .modal-body .book-modal>
            <div .input-group>
                <input type="text" .form-control readonly="" value=@{BookRssR abAuthor abTitle} #copy-rss-input>
                <div .input-group-append>
                    <button .btn.btn-secondary type="button" #copy-rss-button data-clipboard-target="#copy-rss-input">
                        _{MsgCopyRSSLink}
    |]
    withUrlRenderer [hamlet|
        ^{pageBody pc}
    |]

getBookOverlayR :: AbAuthor -> AbTitle -> Handler Html
getBookOverlayR = withBook bookOverlay

getSeriesOverlayR :: AbAuthor -> AbSeries -> Handler Html
getSeriesOverlayR = withSeriesBooks $ \case
        []     -> notFound
        book:_ -> bookOverlay book

bookFeed :: Audiobook -> IO (Feed (Route App))
bookFeed Audiobook{abTitle=abTitle@(AbTitle title), ..} = do
    now <- getCurrentTime
    size <- Directory.getFileSize (T.unpack abPath)
    return Feed
        { feedTitle = title
        , feedLinkSelf = BookRssR abAuthor abTitle
        , feedLinkHome = BookViewR
        , feedAuthor = ""
        , feedDescription = ""
        , feedLanguage = "en"
        , feedUpdated = now
        , feedLogo = Just (BookCoverR abAuthor abTitle, title)
        , feedEntries = [
            FeedEntry
            { feedEntryLink = BookRssR abAuthor abTitle
            , feedEntryUpdated = now
            , feedEntryTitle = title
            , feedEntryContent = ""
            , feedEntryEnclosure = Just $ EntryEnclosure
                { enclosedUrl = BookFileR abAuthor abTitle
                , enclosedSize = fromIntegral size
                , enclosedMimeType = "audio/mpeg"
                }
            }
        ]
        }

getBookRssR :: AbAuthor -> AbTitle -> Handler RepRss
getBookRssR = withBook $ \book -> do
    feed <- liftIO $ bookFeed book
    rssFeed feed
