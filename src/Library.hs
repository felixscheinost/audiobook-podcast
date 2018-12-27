{-# LANGUAGE NoImplicitPrelude #-}

module Library (
    getAudiobookCover,
    isAudioFile,
    audiobookFromFilePath
) where

import           Control.Applicative  (many, (<|>))
import           Data.Attoparsec.Text
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Database.Tables      (Audiobook, AudiobookT (..), abPath)
import           Import.NoFoundation
import           Settings             (AppSettings (appAudioExtensions),
                                       ReadSettings, asksSettings,
                                       runSettingsReader)
import           System.FilePath      (extSeparator, makeRelative,
                                       pathSeparator, takeBaseName,
                                       takeDirectory, takeExtension, (<.>),
                                       (</>))


getAudiobookCover :: Audiobook -> FilePath
getAudiobookCover ab = dir </> base <.> "jpg"
    where
        dir = takeDirectory $ T.unpack $ abPath ab
        base = takeBaseName $ T.unpack $ abPath ab

isAudioFile :: ReadSettings m => m (FilePath -> Bool)
isAudioFile = do
    audioExtensions <- fmap (('.' :) . T.unpack) . appAudioExtensions <$> asksSettings
    return $ \filePath -> takeExtension filePath `elem` audioExtensions

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
        Left err -> return $ Left err

