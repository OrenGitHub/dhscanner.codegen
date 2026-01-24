{-# LANGUAGE OverloadedStrings #-}

module LoggingConfig
    ( logMessage
    , messageLoggerWithoutSource
    , myLogger
    , timeFormat
    ) where

import Prelude
import qualified Data.Text as T
import Data.Time
import Yesod.Core (LogLevel(..))
import qualified Yesod.Core.Types as Y
import System.Log.FastLogger (LogStr, LoggerSet, defaultBufSize, fromLogStr, newStdoutLoggerSet, pushLogStrLn, toLogStr, newTimeCache)
import qualified Data.ByteString.Char8 as BS8

timeFormat :: String
timeFormat = "[%d/%m/%Y ( %H:%M:%S )]"

logMessage :: LoggerSet -> T.Text -> LogLevel -> LogStr -> IO ()
logMessage loggerSetValue _source level msg = do
    timestamp <- formatTimestamp
    let line = assembleLogLine timestamp (formatLevel level) (logStrToString msg)
    pushLogStrLn loggerSetValue (toLogStr line)

messageLoggerWithoutSource :: Y.Logger -> loc -> T.Text -> LogLevel -> LogStr -> IO ()
messageLoggerWithoutSource (Y.Logger loggerSetValue _) _loc = logMessage loggerSetValue

formatTimestamp :: IO String
formatTimestamp = do
    now <- getZonedTime
    pure (formatTime defaultTimeLocale timeFormat now)

formatLevel :: LogLevel -> String
formatLevel level = case level of
    LevelDebug -> "DEBUG"
    LevelInfo -> "INFO"
    LevelWarn -> "WARN"
    LevelError -> "ERROR"
    LevelOther other -> T.unpack other

logStrToString :: LogStr -> String
logStrToString msg = BS8.unpack (fromLogStr msg)

assembleLogLine :: String -> String -> String -> String
assembleLogLine timestamp level msg = timestamp ++ " [" ++ level ++ "] " ++ msg

myLogger :: IO Y.Logger
myLogger = do
    _loggerSet <- newStdoutLoggerSet defaultBufSize
    _formatter <- newTimeCache (BS8.pack timeFormat)
    return $ Y.Logger _loggerSet _formatter
