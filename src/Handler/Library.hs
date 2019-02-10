module Handler.Library (
    getReloadLibraryR
) where

import           Foundation
import           Import
import qualified Library

getReloadLibraryR :: Handler String
getReloadLibraryR = do
    numImported <- Library.reloadLibrary
    return $ "Imported " ++ show (numImported :: Int) ++ " audiobooks"
