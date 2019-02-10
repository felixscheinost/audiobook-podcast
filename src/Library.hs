{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Library (
    getAudiobookCoverPath,
    getSeriesCoverPath,
    isAudioFile,
    audiobookFromFilePath,
    reloadLibrary,
    MonadApplication,
) where

import qualified Codec.Picture            as Picture
import           Control.Applicative      ((<|>))
import           Control.Monad.Catch      (catchIf)
import           Data.Attoparsec.Text
import qualified Data.Conduit.Combinators as CDT
import qualified Data.Conduit.List        as CDTL
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import qualified Data.List.NonEmpty       as NEL
import           Data.Semigroup           (Max (Max), getMax)
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
import           System.IO.Error          (isDoesNotExistError)
import qualified System.Posix.Files       as Posix

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
getAudiobookCoverPath ab = findWithImageExtensions (T.unpack $ abPath ab)

getSeriesDirectory :: NonEmpty Audiobook -> FilePath
getSeriesDirectory = FP.takeDirectory . T.unpack . abPath . NEL.head

getUserSeriesCoverPath :: MonadIO m => NonEmpty Audiobook -> m (Maybe FilePath)
getUserSeriesCoverPath audiobooks = findWithImageExtensions (getSeriesDirectory audiobooks </> "cover")

getGeneratedSeriesCoverPath :: NonEmpty Audiobook -> FilePath
getGeneratedSeriesCoverPath audiobooks = getSeriesDirectory audiobooks </> "cover.generated.jpg"

generateSeriesCover :: (MonadIO m, MonadLogger m) => FilePath -> [FilePath] -> m (Maybe FilePath)
generateSeriesCover savePath abCoverPaths = case abCoverPaths of
    [] -> return Nothing
    x:xs -> do
        collage <- PictureTools.pictureCollage 500 500 (x :| xs)
        maybe (return Nothing) (\img -> liftIO $ Picture.saveJpgImage 85 savePath img >> return (Just savePath)) collage

-- | Return path to cover image to use for a series given the books that are part of this series.
--   Returns Nothing if there is no cover, otherwise the returned file path can be assumed to exist.
getSeriesCoverPath :: (MonadIO m, MonadLogger m) => NonEmpty Audiobook -> m (Maybe FilePath)
getSeriesCoverPath audiobooks = do
    userCoverPath <- getUserSeriesCoverPath audiobooks
    if isJust userCoverPath then
        return userCoverPath
    else do
        abCoverPaths <- catMaybes <$> mapM getAudiobookCoverPath (NEL.toList audiobooks)
        let generatedCoverPath = getGeneratedSeriesCoverPath audiobooks
            -- Returns the unix ctime or nothing if the file does not exist
            getMaybeCTime fp = liftIO
                $ flip (catchIf isDoesNotExistError) (const $ return Nothing)
                $ Just . Posix.statusChangeTime <$> Posix.getFileStatus fp
        abCoverChanges <- liftIO $ mapM getMaybeCTime abCoverPaths
        let latestAbCoverChange = getMax $ foldl' (<>) (Max Nothing) (Max <$> abCoverChanges)
        generatedCoverChange <- liftIO $ getMaybeCTime generatedCoverPath
        putStrLn $ T.pack $ "generatedCoverChange " ++ show generatedCoverChange
        putStrLn $ T.pack $ "latestAbCoverChange " ++ show latestAbCoverChange
        case (latestAbCoverChange, generatedCoverChange) of
            -- no audiobook cover and no generated cover
            (Nothing, Nothing) -> return Nothing
            -- no audiobook cover but generated cover
            (Nothing, Just _)  -> return (Just generatedCoverPath)
            -- at least one audiobook cover but no generated cover
            (Just _, Nothing)  -> generateSeriesCover generatedCoverPath abCoverPaths
            -- at least one audiobook cover and also a generated cover
            (Just coverChange, Just generatedChange) ->
                if coverChange <= generatedChange
                    then return (Just generatedCoverPath)
                    else generateSeriesCover generatedCoverPath abCoverPaths

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

