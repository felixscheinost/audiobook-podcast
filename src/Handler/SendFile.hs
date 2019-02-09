module Handler.SendFile (
    sendFileMime
) where

import qualified Data.Text       as T
import           Import
import           Network.Mime    (defaultMimeLookup)
import           System.FilePath (takeFileName)

-- Respond with a file; Take the mime type from the file extension.
-- This uses the sendfile(2) call on Linux.
sendFileMime :: FilePath -> Handler TypedContent
sendFileMime fp = do
    let mime = defaultMimeLookup $ T.pack $ takeFileName fp
    cacheSeconds (3600 * 12)
    sendFile mime fp
