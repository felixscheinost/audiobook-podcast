module Zip (
    getFiles,
    getSingleFile
) where

import qualified Algorithms.NaturalSort as NS
import qualified Codec.Archive.Zip      as Z
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString        (ByteString)
import           Data.Conduit           (ConduitT)
import           Data.List              (sortBy)
import qualified Data.Map               as M
import Import (Handler, MonadThrow, MonadResource, PrimMonad)

getFiles :: MonadIO m => FilePath -> m [FilePath]
getFiles fp = Z.withArchive fp (processKeys <$> Z.getEntries)
    where
        processKeys = sortBy NS.compare . map Z.unEntrySelector . M.keys

getSingleFile :: FilePath -> FilePath -> IO (ConduitT () ByteString Handler ())
getSingleFile zipFp fileFp = Z.withArchive zipFp $ do
    entry <- Z.mkEntrySelector fileFp
    Z.getEntrySource entry
