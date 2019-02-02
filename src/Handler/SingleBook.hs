{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Handler.SingleBook where

import qualified Data.Text        as T
import           Data.Time.Clock  (getCurrentTime)
import           Database         (AbAuthor, AbTitle (..), Audiobook,
                                   AudiobookT (..))
import qualified Database         as DB
import qualified Handler.SendFile as SendFile
import           Import           hiding (count, fileSize)
import qualified Library
import qualified System.Directory as Directory
import           Yesod.RssFeed    (RepRss, rssFeed)

withBookByAuthorTitle :: (Audiobook -> Handler a) -> AbAuthor -> AbTitle -> Handler a
withBookByAuthorTitle f _author _title = runSQLGetOr404 (DB.getAudiobookByAuthorTitle _author _title) >>= f

withBookById :: (Audiobook -> Handler a) -> Int -> Handler a
withBookById f _id = runSQLGetOr404 (DB.getAudiobookById _id) >>= f

getBookCoverByAuthorTitleR :: AbAuthor -> AbTitle -> Handler TypedContent
getBookCoverByAuthorTitleR = withBookByAuthorTitle (SendFile.sendFileMime . Library.getAudiobookCover)

getBookCoverByIdR :: Int -> Handler TypedContent
getBookCoverByIdR = withBookById (SendFile.sendFileMime . Library.getAudiobookCover)

getBookFileR :: AbAuthor -> AbTitle -> Handler TypedContent
getBookFileR = withBookByAuthorTitle (SendFile.sendFileMime . T.unpack . abPath)

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

getBookOverlayByAuthorTitleR :: AbAuthor -> AbTitle -> Handler Html
getBookOverlayByAuthorTitleR = withBookByAuthorTitle bookOverlay

getBookOverlayByIdR :: Int -> Handler Html
getBookOverlayByIdR = withBookById bookOverlay

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
        , feedLogo = Just (BookCoverByAuthorTitleR abAuthor abTitle, title)
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
getBookRssR = withBookByAuthorTitle $ \book -> do
    feed <- liftIO $ bookFeed book
    rssFeed feed
