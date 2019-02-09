{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

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
import           Database                 (AbAuthor (..), AbSeries (..),
                                           AbTitle (..), Audiobook,
                                           AudiobookT (..), NewAudiobook (..),
                                           RunSQL, runSQL)
import qualified Database
import           Database.Beam
import           Import.NoFoundation
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

extension :: ReaderT (AppSettings, FilePath) Parser ()
extension = do
    audioExtensions <- asks (appAudioExtensions . fst)
    lift $ do
        skip (== '.')
        choice (string <$> audioExtensions) $> ()

parseATS :: Text -> ReaderT (AppSettings, FilePath) Parser NewAudiobook
parseATS author = do
    series <- lift folder
    lift $ skipMany space
    seriesIndex <- lift decimal
    lift (skipWhile ((||) <$> (== ' ') <*> (== '-')))
    title <- lift upToExtension
    extension
    abPath <- asks snd
    return $ NewAudiobook { nabAuthor=AbAuthor author
                          , nabPath=T.pack abPath
                          , nabTitle=AbTitle title
                          , nabSeries=Just (AbSeries series)
                          , nabSeriesIndex=Just seriesIndex
                          }

parseAT :: Text -> ReaderT (AppSettings, FilePath) Parser NewAudiobook
parseAT author = do
    title <- lift upToExtension
    extension
    abPath <- asks snd
    return $ NewAudiobook { nabAuthor=AbAuthor author
                          , nabPath=T.pack abPath
                          , nabTitle=AbTitle title
                          , nabSeries=Nothing
                          , nabSeriesIndex=Nothing
                          }

parseAudiobook :: ReaderT (AppSettings, FilePath) Parser NewAudiobook
parseAudiobook = do
    author <- lift folder
    parseAT author <|> parseATS author

audiobookFromFilePath :: ReadSettings m => FilePath -> m (Either String NewAudiobook)
audiobookFromFilePath filePath = do
    libraryPath <- appLibraryFolder <$> asksSettings
    let relativeFilePath = T.pack $ makeRelative libraryPath filePath
    appSettings <- asksSettings
    let parser = runReaderT parseAudiobook (appSettings, filePath)
    let abOrError = parseOnly (parser <* endOfInput) relativeFilePath
    case abOrError of
        Right ab -> return $ Right ab
        Left err -> return $ Left $ "Error parsing " ++ T.unpack relativeFilePath ++ ": " ++ err

