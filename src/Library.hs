{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Library (
    getAudiobookCover,
    isAudioFile,
    audiobookFromFilePath,
    reloadLibrary,
    MonadApplication
) where

import           Control.Applicative      ((<|>))
import           Data.Attoparsec.Text
import qualified Data.Conduit.Combinators as CDT
import qualified Data.Conduit.List        as CDTL
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Database                 (AppDbConnection, Audiobook,
                                           AudiobookT (..), RunSQL, runSQL)
import qualified Database
import           Database.Beam
import           Import.NoFoundation
import           Settings                 (AppSettings (appAudioExtensions),
                                           ReadSettings, asksSettings,
                                           runSettingsReader)
import           System.FilePath          (extSeparator, makeRelative,
                                           pathSeparator, takeBaseName,
                                           takeDirectory, takeExtension, (<.>))

-- TODO: Need to move this someplace else if I start using it outside of this file
type MonadApplication m = (MonadIO m, ReadSettings m, RunSQL m, MonadLogger m, MonadResource m)

getAudiobookCover :: Audiobook -> FilePath
getAudiobookCover ab = dir </> base <.> "jpg"
    where
        dir = takeDirectory $ T.unpack $ abPath ab
        base = takeBaseName $ T.unpack $ abPath ab

isAudioFile :: ReadSettings m => m (FilePath -> Bool)
isAudioFile = do
    audioExtensions <- fmap (('.' :) . T.unpack) . appAudioExtensions <$> asksSettings
    return $ \filePath -> takeExtension filePath `elem` audioExtensions

reloadLibrary :: MonadApplication m => m Int
reloadLibrary = do
    libraryFolder <- appLibraryFolder <$> asksSettings
    isAudio <- Library.isAudioFile
    runSQL Database.deleteAllAudiobooks
    let conduit = CDT.sourceDirectoryDeep True libraryFolder
            .| CDT.filter isAudio
            .| CDT.mapM Library.audiobookFromFilePath
            .| CDTL.mapMaybeM (\case
                    Left err -> logErrorN (T.pack err) >> return Nothing
                    Right ab -> return (Just ab)
                )
            .| CDT.iterM (runSQL . Database.insertAudiobook)
            .| CDT.length
    runConduit conduit

-- ======================
--  Parse Info from Path
-- ======================

folder :: Parser Text
folder = do
    name <- takeWhile1 (/= pathSeparator)
    _ <- char pathSeparator
    return name

upToExtension :: Parser Text
upToExtension = takeWhile1 ((&&) <$> (/= extSeparator) <*> (/= pathSeparator))

extension :: ReaderT AppSettings Parser ()
extension = do
    audioExtensions <- asks appAudioExtensions
    lift $ do
        skip (== '.')
        choice (string <$> audioExtensions) $> ()

parseATS :: Text -> ReaderT AppSettings Parser Audiobook
parseATS author = do
    series <- lift folder
    lift $ skipMany space
    seriesIndex <- lift decimal
    lift (skipMany space)
    title <- lift upToExtension
    extension
    return $ Audiobook { abAuthor=author
                       , abTitle=title
                       , abSeries=Just series
                       , abSeriesIndex=Just seriesIndex
                       }

parseAT :: Text -> ReaderT AppSettings Parser Audiobook
parseAT author = do
    title <- lift upToExtension
    extension
    return $ Audiobook { abAuthor=author
                       , abTitle=title
                       , abSeries=Nothing
                       , abSeriesIndex=Nothing
                       }

parseAudiobook :: ReaderT AppSettings Parser Audiobook
parseAudiobook = do
    author <- lift folder
    parseAT author <|> parseATS author

audiobookFromFilePath :: ReadSettings m => FilePath -> m (Either String Audiobook)
audiobookFromFilePath filePath = do
    libraryPath <- appLibraryFolder <$> asksSettings
    let relativeFilePath = T.pack $ makeRelative libraryPath filePath
    parser <- runSettingsReader parseAudiobook
    let abOrError = parseOnly (parser <* endOfInput) relativeFilePath
    case abOrError of
        Right ab -> return $ Right ab{abPath=T.pack filePath}
        Left err -> return $ Left $ "Error parsing " ++ T.unpack relativeFilePath ++ ": " ++ err

