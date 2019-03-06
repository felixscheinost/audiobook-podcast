{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Series where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text          as T
import qualified Data.Time.Clock    as Time
import           Database           (AbAuthor (..), AbSeries (..), AbTitle (..),
                                     Audiobook, AudiobookT (..))
import qualified Database
import           Foundation
import qualified Handler.SendFile   as SendFile
import           Import
import           Library            (SeriesCover (..))
import qualified Library
import qualified System.Directory   as Directory
import           Yesod.RssFeed      (RepRss, rssFeed)

getSeriesViewR :: AbAuthor -> AbSeries -> Handler Html
getSeriesViewR abAuthor abSeries = do
    pc <- widgetToPageContent [whamlet|
        <div .modal.fade tabindex="-1" role="dialog" aria-hidden="true">
            <div .modal-dialog.modal-lg>
                <div .modal-content>
                    <div .modal-header>
                        <h5 .modal-title> #{abSeries}
                        <button .close type="button" data-dismiss="modal" aria-label="Close">
                            <span aria-hidden="true">&times;
                    <div .modal-body .book-modal>
                        <div .input-group>
                            <input type="text" .form-control readonly="" value=@{SeriesRssR abAuthor abSeries} #copy-rss-input>
                            <div .input-group-append>
                                <button .btn.btn-secondary type="button" #copy-rss-button data-clipboard-target="#copy-rss-input">
                                    _{MsgCopyRSSLink}
    |]
    withUrlRenderer [hamlet|
        ^{pageBody pc}
    |]

getSeriesCoverR :: AbAuthor -> AbSeries -> Handler TypedContent
getSeriesCoverR abAuthor abSeries = do
    maybeBooks <- runSQL (Database.getAudiobooksByAuthorSeries abAuthor abSeries)
    cover <- Library.getSeriesCover maybeBooks
    case cover of
        MissingSeriesCover -> redirect (StaticR img_cover_placeholder_svg)
        GeneratedGrid audiobooksAndPaths -> do
            generated <- Library.generateSeriesCoverJpeg (snd <$> audiobooksAndPaths)
            maybe (redirect $ StaticR img_cover_placeholder_svg) (respond typeJpeg) generated
        FromPath path -> SendFile.sendFileMime path

seriesFeed :: AbAuthor -> AbSeries -> NonEmpty Audiobook -> IO (Feed (Route App))
seriesFeed abAuthor@(AbAuthor author) abSeries@(AbSeries series) books = do
    seriesCover <- Library.getSeriesCover (NEL.toList books)
    let feedLogo = case seriesCover of
            MissingSeriesCover -> Nothing
            _                  -> Just (SeriesCoverR abAuthor abSeries, series)
    now <- Time.getCurrentTime
    entries <- forM (zip [0, -1..] $ NEL.toList books) $ \(i, Audiobook{abTitle=abTitle@(AbTitle title), abPath}) -> do
        size <- Directory.getFileSize (T.unpack abPath)
        return FeedEntry
            { feedEntryLink = BookRssR abAuthor abTitle
            , feedEntryUpdated = Time.addUTCTime (fromIntegral i) now
            , feedEntryTitle = title
            , feedEntryContent = ""
            , feedEntryEnclosure = Just $ EntryEnclosure
                { enclosedUrl = BookFileR abAuthor abTitle
                , enclosedSize = fromIntegral size
                , enclosedMimeType = "audio/mpeg"
                }
            }
    return Feed
        { feedTitle = series
        , feedLinkSelf = SeriesRssR abAuthor abSeries
        , feedLinkHome = BookViewR
        , feedAuthor = author
        , feedDescription = ""
        , feedLanguage = "en"
        , feedUpdated = now
        , feedLogo = feedLogo
        , feedEntries = entries
        }

getSeriesRssR :: AbAuthor -> AbSeries -> Handler RepRss
getSeriesRssR abAuthor abSeries = do
    maybeBooks <- runSQL (Database.getAudiobooksByAuthorSeries abAuthor abSeries)
    books <- maybe notFound return $ NEL.nonEmpty maybeBooks
    feed <- liftIO $ seriesFeed abAuthor abSeries books
    rssFeed feed

