module Library (
    getAudiobookCover
) where

import qualified Data.Text       as T
import           Database.Tables (Audiobook, abPath)
import           System.FilePath (takeBaseName, takeDirectory, (<.>), (</>))

getAudiobookCover :: Audiobook -> FilePath
getAudiobookCover ab = dir </> base <.> "jpg"
    where
        dir = takeDirectory (T.unpack $ abPath ab)
        base = takeBaseName (T.unpack $ abPath ab)
