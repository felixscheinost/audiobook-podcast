{-# LANGUAGE PackageImports #-}
import "calibre-audiobook-podcast" Application (develMain)
import Prelude (IO)

-- this file is needed for yesod-devel
main :: IO ()
main = develMain