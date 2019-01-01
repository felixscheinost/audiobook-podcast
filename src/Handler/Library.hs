{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Library (
    getReloadLibraryR
) where

import qualified Data.Conduit             as CDT
import qualified Data.Conduit.Combinators as CDT
import qualified Data.Conduit.List        as CDTL
import qualified Data.Text                as T
import           Database                 (Audiobook, AudiobookT (..))
import qualified Database
import           Foundation
import           Import
import qualified Library
import qualified System.Directory         as Dir
import           System.FilePath          ((</>))

getReloadLibraryR :: Handler String
getReloadLibraryR = do
    libraryFolder <- appLibraryFolder <$> asksSettings
    predicate <- Library.isAudioFile
    runSQL Database.deleteAllAudiobooks
    liftIO (Dir.listDirectory libraryFolder >>= print)
    let conduit = CDT.sourceDirectoryDeep True libraryFolder
            .| CDT.filter predicate
            .| CDT.mapM Library.audiobookFromFilePath
            .| CDTL.mapMaybeM (\case
                    Left err -> logErrorN (T.pack err) >> return Nothing
                    Right ab -> return (Just ab)
                )
            .| CDT.iterM (runSQL . Database.insertAudiobook)
            .| CDT.length
    sum <- runConduit conduit
    return $ "Imported " ++ show (sum :: Int) ++ " audiobooks"
