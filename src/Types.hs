module Types where

import           Data.Text (Text)
import qualified Data.List as L
import qualified Data.Char as C
import qualified System.FilePath as FP

data AudioFormat
    = Mp3
    | M4a
    | M4b
    deriving (Show, Read, Eq, Enum)

supportedAudioFormats :: [AudioFormat]
supportedAudioFormats = enumFrom $ toEnum 0

audioFileExtension :: AudioFormat -> String
audioFileExtension = fmap C.toLower <$> show

filePathAudioFormat :: FilePath -> Maybe AudioFormat
filePathAudioFormat fp = L.find (correctExtension . audioFileExtension) supportedAudioFormats
    where
        correctExtension = (== FP.takeExtension fp)

ffmpegFormatStr :: AudioFormat -> String
ffmpegFormatStr = audioFileExtension

data Audiobook = Audiobook
    { abId     :: Int
    , abTitle  :: Text
    , abPath   :: FilePath
    , abFormat :: AudioFormat
    , abCover  :: FilePath
    }
