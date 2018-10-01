{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Calibre.Types where

import Database.Beam
import Database.Beam.Sqlite
    
data AudioFormat =
    MP3
    deriving (Show, Read, Eq, Enum)

data AudiobookFormat
    = SingleFile AudioFormat
    | ZIP
    deriving (Show, Read, Eq) 

-- instance FromBackendRow Sqlite AudiobookFormat