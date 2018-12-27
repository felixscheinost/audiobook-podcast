{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Library (
    getReloadLibraryR
) where

import qualified Data.ByteString.Builder  as BSB
import qualified Data.Conduit             as CDT
import qualified Data.Conduit.Combinators as CDT
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as E
import           Database                 (Audiobook, AudiobookT (..))
import           Foundation
import           Import
import qualified Library
import qualified System.Directory         as Dir
import           System.FilePath          ((</>))


myPrint :: Either String Audiobook -> Text
myPrint (Left err)                         = T.pack err
myPrint (Right Audiobook{abTitle=abTitle}) = abTitle

getReloadLibraryR :: Handler TypedContent
getReloadLibraryR = do
    libraryFolder <- appLibraryFolder <$> asksSettings
    predicate <- Library.isAudioFile
    let cdt = CDT.sourceDirectoryDeep True libraryFolder
            .| CDT.filter predicate
            .| CDT.mapM Library.audiobookFromFilePath
    respondSource "text/plain" $
        CDT.mapOutput (Chunk . (<> BSB.char8 '\n') . BSB.byteString . E.encodeUtf8 . myPrint) cdt

