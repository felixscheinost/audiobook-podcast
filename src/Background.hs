{-# LANGUAGE RecordWildCards #-}

module Background where

import Import
import Background.Conversion (scanBooksToConvertJob)
import Background.Foundation
import Control.Concurrent (forkIO)

forkBackgroundJobs :: App -> IO ()
forkBackgroundJobs App{..} = do
   let b = Background 
        { backgroundSettings = appSettings
        , backgroundDbConnection = appDbConnection 
        , backgroundLogger = appLogger
        } 
   _ <- forkIO $ runBackground b scanBooksToConvertJob
   return ()