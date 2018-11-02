{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import           Control.Monad.Logger                 (liftLoc)
import           Background                           (forkBackgroundJobs)
import qualified Data.CaseInsensitive                 as CI
import           Database.Calibre
import qualified Database.SQLite.Simple               as Sql
import           Import                               hiding (requestHeaders)
import           Language.Haskell.TH.Syntax           (qLocation)
import           Network.Wai                          (Middleware, Request (..))
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       defaultShouldDisplayException,
                                                       getPort, runSettings,
                                                       setOnException, setPort)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (OutputFormat (CustomOutputFormat, Apache),
                                                       OutputFormatter, IPAddrSource(FromSocket),
                                                       RequestLoggerSettings (outputFormat),
                                                       mkRequestLogger)
import           System.Log.FastLogger                (defaultBufSize,
                                                       newStdoutLoggerSet,
                                                       toLogStr)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file
import           Controllers.Book
import           Controllers.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    let appStatic = myStatic
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appDbConnection <- Sql.open (libraryPath appSettings) >>= newMVar
    return $ App {..}

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    return $
        gzip def $
        logWare $
        defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    if appDevelopment $ appSettings foundation then
        mkRequestLogger $ def {
            outputFormat =  CustomOutputFormat logWareFormat
        }
    else
        return id
        --mkRequestLogger def { outputFormat = Apache FromSocket }

logWareFormat :: OutputFormatter
logWareFormat _ req status _ =
    toLogStr (requestMethod req) <> toLogStr (" " :: Text)
    <> toLogStr (rawPathInfo req) <> toLogStr ("\n" :: Text)
    <> mconcat (logHeader <$> headers)
    <> toLogStr ("\t => " :: Text) <> toLogStr (show $ statusCode status) <> toLogStr ("\n" :: Text)
    where
        headers = ["Range"]
        logHeader h = fromMaybe mempty $ do
            value <- lookup h (requestHeaders req)
            return $ toLogStr ("\t" :: Text)
                <> toLogStr (CI.original h)
                <> toLogStr (": " :: Text)
                <> toLogStr value
                <> toLogStr ("\n" :: Text)

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

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper $ do
    foundation <- getAppSettings >>= makeFoundation
    app <- makeApplication foundation
    devWsettings <- getDevSettings $ warpSettings foundation
    return (devWsettings, app)

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    foundation <- getAppSettings >>= makeFoundation
    app <- makeApplication foundation
    let wsettings = warpSettings foundation
    forkBackgroundJobs foundation
    putStrLn $ "Running on port " ++ tshow (getPort wsettings)
    runSettings wsettings app
