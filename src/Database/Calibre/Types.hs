module Database.Calibre.Types where
    
data AudioFormat =
    MP3
    deriving (Show, Read, Eq, Enum)

data AudiobookFormat
    = SingleFile AudioFormat
    | ZIP
    deriving (Show, Read, Eq) 