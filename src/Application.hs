{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import           Control.Monad.Logger                 (liftLoc, runLoggingT)
import           Import
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       defaultShouldDisplayException,
                                                       getPort, runSettings,
                                                       setHost, setOnException,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       destination,
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file
import           Controllers.Calibre
import           Controllers.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details
mkYesodDispatch "App" resourcesApp

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    static_ <- static "static"
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    return $ App appSettings static_ appLogger

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat = Apache FromSocket
        , destination = Logger $ loggerSet $ appLogger foundation
        }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    -- $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) 
            $ messageLoggerSource foundation (appLogger foundation)
            $(qLocation >>= liftLoc) "yesod" LevelError (toLogStr $ "Exception from Warp: " ++ show e))
        defaultSettings

getAppAndWarpSettings :: IO (Settings, Application)
getAppAndWarpSettings = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    app <- makeApplication foundation
    wsettings <- getDevSettings $ warpSettings foundation
    return (wsettings, app)
    
-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    app <- makeApplication foundation
    wsettings <- getDevSettings $ warpSettings foundation
    return (wsettings, app)

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper $ do
    (wsettings, app) <- getAppAndWarpSettings
    devWsettings <- getDevSettings wsettings
    return $ (devWsettings, app)

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    (wsettings, app) <- getAppAndWarpSettings
    runSettings wsettings app