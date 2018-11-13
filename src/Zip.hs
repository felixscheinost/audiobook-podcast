module Zip (
    getFilePathsInZip,
    saveEntry,
    unpackInto,
    UnzipProgress
) where

import qualified Codec.Archive.Zip        as Zip
import           Control.Monad
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.ByteString          as BS
import           Data.Conduit             ((.|))
import qualified Data.Conduit             as Conduit
import qualified Data.Conduit.Binary      as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           GHC.Exts                 (toList)
import           System.Directory
import           System.FilePath          ((</>))
import qualified System.FilePath          as FP

getFilePathsInZip :: MonadIO m => FilePath -> m [FilePath]
getFilePathsInZip fp = Zip.withArchive fp (fmap Zip.unEntrySelector . Map.keys <$> Zip.getEntries)

type UnzipProgress = (Integer, Integer)

-- | A version of Zip.saveEntry that can monitor the progress of the operation
saveEntry :: Zip.EntrySelector -> FilePath -> ((UnzipProgress -> UnzipProgress) -> IO ()) -> Zip.ZipArchive ()
saveEntry s path updateProgress = do
    let sink = Conduit.sinkFileCautious path
    let updateProgressPipe = Conduit.iterM $ \bytes -> liftIO  $ updateProgress $
            \(delta, total) -> (delta + fromIntegral (BS.length bytes), total)
    source <- Zip.getEntrySource s
    liftIO $ Conduit.runConduitRes (source .| updateProgressPipe .| sink)
    med <- Zip.getEntryDesc s
    forM_ med (liftIO . setModificationTime path . Zip.edModTime)

-- | A version of Zip.unpackInto that can monitor the progress of the operation and returns the filepaths of the created files
unpackInto :: FilePath -> FilePath -> ((Maybe UnzipProgress -> Maybe UnzipProgress) -> IO ()) -> IO [FilePath]
unpackInto dir' zipPath updateProgress = Zip.withArchive zipPath $ do
    selectors <- Map.keysSet <$> Zip.getEntries
    if null selectors then
        return []
    else do
        dir <- liftIO (makeAbsolute dir')
        liftIO (createDirectoryIfMissing True dir)
        let dirs =
                Set.map
                    (FP.takeDirectory . (dir </>) . Zip.unEntrySelector)
                    selectors
        forM_ dirs (liftIO . createDirectoryIfMissing True)
        let entrySize s = maybe 0 Zip.edUncompressedSize <$> Zip.getEntryDesc s
        totalSize <- foldM (\size sel -> (size +) <$> entrySize sel) 0 selectors
        liftIO $ updateProgress $ const $ Just (fromIntegral totalSize, 0)
        forM (toList selectors) $ \s -> do
            saveEntry s (dir </> Zip.unEntrySelector s) (updateProgress . fmap)
            return $ dir </> Zip.unEntrySelector s
