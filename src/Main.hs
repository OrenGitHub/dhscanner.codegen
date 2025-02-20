{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-unused-matches   #-}
{-# OPTIONS -Wno-unused-top-binds #-}

import Yesod
import Prelude
import Data.Set
import Data.Aeson()
import GHC.Generics
import Data.Text
import Data.Time
import Data.List
import Data.Maybe
import Yesod.Core.Types
import System.Log.FastLogger
import Network.Wai.Handler.Warp

-- Wai stuff
import qualified Network.Wai
import qualified Network.Wai.Logger
import qualified Network.HTTP.Types.Status
import qualified Network.Wai.Middleware.RequestLogger as Wai

-- project imports
import Cfg
import Asts
import Location
import Callable
import qualified Bitcode
import CodeGen ( codeGen )

-- general imports
import GHC.Generics

data Healthy = Healthy Bool deriving ( Generic )

instance ToJSON Healthy where toJSON (Healthy status) = object [ "healthy" .= status ]

data App = App

mkYesod "App" [parseRoutes|
/codegen HomeR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App where maximumContentLength = \app -> (\anyRouteReally -> Just 64000000)

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

keepMethod :: Callable -> Maybe MethodContent
keepMethod (Method m) = Just m
keepMethod _ = Nothing

keepMethods :: [ Callable ] -> [ MethodContent ]
keepMethods callables = catMaybes (Data.List.map keepMethod callables)

-- Change the type predicate as needed ...
-- This is a temporary filter on the logs
-- Come up with an alternative way soon !
relevantOrNothing :: Bitcode.Instruction -> Maybe Bitcode.Instruction
relevantOrNothing instruction@(Bitcode.Instruction _ (Bitcode.Binop  content)) = Just instruction
relevantOrNothing instruction@(Bitcode.Instruction _ (Bitcode.Call   content)) = Just instruction
relevantOrNothing instruction@(Bitcode.Instruction _ (Bitcode.Assign content)) = Just instruction
relevantOrNothing _ = Nothing

extractRelevant :: [ Bitcode.Instruction ] -> [ Bitcode.Instruction ]
extractRelevant instructions = catMaybes (Data.List.map relevantOrNothing instructions)

getInstructions' :: Cfg -> [ Bitcode.Instruction ]
getInstructions' cfg = let
    nodes = Cfg.actualNodes (Cfg.nodes cfg)
    in Data.Set.toList (Data.Set.map Cfg.theInstructionInside nodes)

getInstructions :: [ Cfg ] -> [ Bitcode.Instruction ]
getInstructions cfgs = Data.List.foldl' (++) [] (Data.List.map getInstructions' cfgs)

focusOnRelevantLocation' :: Bitcode.Instruction -> Bool
focusOnRelevantLocation' instruction = let
    l = Bitcode.location instruction
    lineMatches = ((Location.lineStart l) == 32) && ((Location.lineEnd l) == 32)
    colsMatch = True -- ((Location.colStart l) == 22) && ((Location.colEnd l) == 27)
    in lineMatches && colsMatch

focusOnRelevantLocation :: [ Bitcode.Instruction ] -> [ Bitcode.Instruction ]
focusOnRelevantLocation = Data.List.filter focusOnRelevantLocation'

filterRelavantFrom :: Callables -> [ Bitcode.Instruction ]
filterRelavantFrom (Callables callables) = let
    methods = keepMethods callables
    allInstructions = getInstructions (Data.List.map methodBody methods)
    relevantInstructions = extractRelevant allInstructions
    in focusOnRelevantLocation relevantInstructions

postHomeR :: Handler Value
postHomeR = do
    _asts <- requireCheckJsonBody :: Handler Asts
    let result = codeGen _asts
    -- code gen textual output is *huge*
    -- even powerful editors like vim choke on it
    -- until I find a better way, I will let the
    -- server do the log filtering ... 😮
    let instructions = filterRelavantFrom result
    $logInfoS "(CodeGen)" (Data.Text.pack (show instructions))
    returnJson result

myLogger :: IO Logger
myLogger = do
    _loggerSet <- newStdoutLoggerSet defaultBufSize
    formatter <- newTimeCache "[%d/%m/%Y ( %H:%M:%S )]"
    return $ Logger _loggerSet formatter

dateFormatter :: String -> String
dateFormatter date = let
    date' = parseTimeOrError True defaultTimeLocale "%d/%b/%Y:%T %Z" date :: UTCTime
    in formatTime defaultTimeLocale "[%d/%m/%Y ( %H:%M:%S )]" date'

unquote :: String -> String
unquote s = let n = Prelude.length s in Prelude.take (n-2) (Prelude.drop 1 s)

logify :: String -> Network.Wai.Request -> String
logify date req = let
    datePart = dateFormatter date
    method = unquote (show (Network.Wai.requestMethod req))
    url = unquote (show (Network.Wai.rawPathInfo req))
    in datePart ++ " [Info#(Wai)] " ++ method ++ " " ++ url ++ "\n"

formatter :: Network.Wai.Logger.ZonedDate -> Network.Wai.Request -> Network.HTTP.Types.Status.Status -> Maybe Integer -> LogStr
formatter zonedDate req status responseSize = toLogStr (logify (unquote (show zonedDate)) req)

loggerSettings :: Wai.RequestLoggerSettings
loggerSettings = Wai.defaultRequestLoggerSettings { Wai.outputFormat = Wai.CustomOutputFormat formatter }

main :: IO ()
main = do
    waiApp <- toWaiAppPlain App
    myLoggingMiddleware <- Wai.mkRequestLogger loggerSettings
    let middleware = myLoggingMiddleware . defaultMiddlewaresNoLogging
    run 3000 $ middleware waiApp
