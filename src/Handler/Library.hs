{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Library (
    getReloadLibraryR
) where

import qualified Data.ByteString.Builder  as BSB
import qualified Data.Conduit             as CDT
import qualified Data.Conduit.Combinators as CDT
import qualified Data.Conduit.List        as CDTL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E
import           Database                 (Audiobook, AudiobookT (..))
import           Foundation
import           Import
import qualified Library
import qualified System.Directory         as Dir
import           System.FilePath          ((</>))

p :: Audiobook -> Text
p = abTitle

getReloadLibraryR :: Handler TypedContent
getReloadLibraryR = do
    libraryFolder <- appLibraryFolder <$> asksSettings
    predicate <- Library.isAudioFile
    let cdt = CDT.sourceDirectoryDeep True libraryFolder
            .| CDT.filter predicate
            .| CDT.mapM Library.audiobookFromFilePath
            .| CDTL.mapMaybeM (\case
                    Left err -> logErrorN (T.pack err) >> return Nothing
                    Right ab -> return (Just ab)
                )
            .| CDT.iterM (runSQL . Library.insertAudiobook)
    respondSource "text/plain" $
        CDT.mapOutput (Chunk . (<> BSB.char8 '\n') . BSB.byteString . E.encodeUtf8 . p) cdt

