{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Library (
    getAudiobookCoverPath,
    isAudioFile,
    audiobookFromFilePath,
    reloadLibrary,
    generateSeriesCoverJpeg,
    getSeriesCover,
    MonadApplication,
    SeriesCover(..)
) where

import qualified Codec.Picture.Saving     as Picture
import           Control.Applicative      ((<|>))
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy     as Lazy
import qualified Data.Conduit.Combinators as CDT
import qualified Data.Conduit.List        as CDTL
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import qualified Data.List.NonEmpty       as NEL
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Database                 (AbAuthor (..), AbSeries (..),
                                           AbTitle (..), Audiobook,
                                           AudiobookT (..), NewAudiobook (..),
                                           RunSQL, runSQL)
import qualified Database
import           Database.Beam
import           Import.NoFoundation
import qualified PictureTools
import qualified System.Directory         as D
import           System.FilePath          ((</>))
import qualified System.FilePath          as FP

-- TODO: Need to move this someplace else if I start using it outside of this file
type MonadApplication m = (MonadIO m, ReadSettings m, RunSQL m, MonadLogger m, MonadResource m)

-- | Recognized extensions of cover image files
imageExtensions :: [FilePath]
imageExtensions = ["jpg", "jpeg", "png"]

-- | Searches for files where `path` is replaced by a supported image extension.
findWithImageExtensions :: MonadIO m => FilePath -> m (Maybe FilePath)
findWithImageExtensions path = liftIO $ findM D.doesFileExist possiblePaths
    where possiblePaths = FP.replaceExtension path <$> imageExtensions

getAudiobookCoverPath :: MonadIO m => Audiobook -> m (Maybe FilePath)
getAudiobookCoverPath audiobook = findWithImageExtensions (T.unpack $ abPath audiobook)

getSeriesDirectory :: Audiobook -> FilePath
getSeriesDirectory = FP.takeDirectory . T.unpack . abPath

getUserSeriesCoverPath :: MonadIO m => Audiobook -> m (Maybe FilePath)
getUserSeriesCoverPath audiobook = findWithImageExtensions (getSeriesDirectory audiobook </> "cover")

generateSeriesCoverJpeg :: (MonadIO m, MonadLogger m) => NonEmpty FilePath -> m (Maybe Lazy.ByteString)
generateSeriesCoverJpeg paths = fmap (Picture.imageToJpg 85) <$> PictureTools.pictureCollage 500 500 paths

data SeriesCover
    = FromPath FilePath
    | GeneratedGrid (NonEmpty (Audiobook, FilePath))
    | MissingSeriesCover

getSeriesCover :: MonadIO m => [Audiobook] -> m SeriesCover
getSeriesCover audiobooks = do
    userCoverPath <- maybe (return Nothing) getUserSeriesCoverPath (headMay audiobooks)
    let audiobookAndCover audiobook = fmap (audiobook, ) <$> getAudiobookCoverPath audiobook
    audiobookCoverPaths <- NEL.nonEmpty . catMaybes <$> mapM audiobookAndCover audiobooks
    return $ case (userCoverPath, audiobookCoverPaths) of
        (Just path, _)                 -> FromPath path
        (Nothing, Just (a :| b:c:d:_)) -> GeneratedGrid (a :| [b, c, d])
        (Nothing, Just (a :| _))       -> FromPath (snd a)
        (Nothing, Nothing)             -> MissingSeriesCover

isAudioFile :: ReadSettings m => m (FilePath -> Bool)
isAudioFile = do
    audioExtensions <- fmap (('.' :) . T.unpack) . appAudioExtensions <$> asksSettings
    return $ \filePath -> FP.takeExtension filePath `elem` audioExtensions

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
    name <- takeWhile1 (/= FP.pathSeparator)
    _ <- char FP.pathSeparator
    return name

upToExtension :: Parser Text
upToExtension = takeWhile1 ((&&) <$> (/= FP.extSeparator) <*> (/= FP.pathSeparator))

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
    let relativeFilePath = T.pack $ FP.makeRelative libraryPath filePath
    appSettings <- asksSettings
    let parser = runReaderT parseAudiobook (appSettings, filePath)
    let abOrError = parseOnly (parser <* endOfInput) relativeFilePath
    case abOrError of
        Right ab -> return $ Right ab
        Left err -> return $ Left $ "Error parsing " ++ T.unpack relativeFilePath ++ ": " ++ err

