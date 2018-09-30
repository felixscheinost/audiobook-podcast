{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Controllers.Calibre where

import Foundation
import Yesod.Core
import Database.Calibre

getBookCoverR :: Int -> Handler Html
getBookCoverR id = defaultLayout $ do
    setTitle "Minimal Multifile"
    [whamlet|
        <p>
            Cover for #{id}
    |]
