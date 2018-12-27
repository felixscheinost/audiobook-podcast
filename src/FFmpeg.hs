{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module FFmpeg (
    getDuration
) where

import           Control.Exception.Safe
import           Control.Monad
import           Data.Aeson
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TextEncoding
import           GHC.Generics
import           System.Exit               (ExitCode (ExitSuccess))
import qualified System.IO.Error           as IOError
import qualified System.Process.ByteString as Process

newtype FFprobeOutput = FFprobeOutput
    { format :: FFprobeFormatOutput
    } deriving (Generic, Show)

instance FromJSON FFprobeOutput

data FFprobeFormatOutput = FFprobeFormatOutput
    { filename    :: Text
    , format_name :: Text
    , duration    :: String
    , bit_rate    :: String
    } deriving (Generic, Show)

instance FromJSON FFprobeFormatOutput

data FFmpegException
    = FFmpegNotFound
    | FFprobeNotFound
    | FFmpegNonZeroExit Text
    | FFprobeUnexpectedOutput String Text

instance Show FFmpegException where
    show FFmpegNotFound = "ffmpeg executable not found. Is it installed on your system?"
    show FFprobeNotFound = "ffprobe executable not found. Is it installed on your system?"
    show (FFmpegNonZeroExit stderr) = "FFmpeg returned non-zero return code with error message: " ++ T.unpack stderr
    show (FFprobeUnexpectedOutput parseErrorMsg stdout) = "FFprobe returned unexpected output: " ++ T.unpack stdout ++ "\n parse Error: " ++ parseErrorMsg

instance Exception FFmpegException

getDurationThrows :: FilePath -> IO Double
getDurationThrows fp = do
    (exitCode, stdout, stderr) <- Process.readProcessWithExitCode
        "ffprobe"
        [ "-v", "error"
        , "-print_format", "json"
        , "-show_format"
        , fp
        ]
        ""
    when (exitCode /= ExitSuccess) (throw $ FFmpegNonZeroExit $ TextEncoding.decodeUtf8 stderr)
    dec <- case eitherDecodeStrict stdout of
        Left parseErrorMsg -> throw $ FFprobeUnexpectedOutput parseErrorMsg $ TextEncoding.decodeUtf8 stdout
        Right dec -> return dec
    return (read $ duration $ format dec)

handleFFprobeException :: IOException -> IO a
handleFFprobeException e
    | IOError.isDoesNotExistError e = throw FFprobeNotFound
    | otherwise = throw e

getDuration :: FilePath -> IO Double
getDuration = (`catchIO` handleFFprobeException) . getDurationThrows
