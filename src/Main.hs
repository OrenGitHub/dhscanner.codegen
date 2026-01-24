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
import Data.Aeson()
import qualified Data.Text as T
import Network.Wai.Handler.Warp

-- project imports
import Ast
import Callable ( actualCallables )
import CodeGen ( codeGen )
import LoggingConfig (messageLoggerWithoutSource, myLogger)

-- general imports
import GHC.Generics

data Healthy = Healthy Bool deriving ( Generic )

instance ToJSON Healthy where toJSON (Healthy status) = object [ "healthy" .= status ]

data App = App

mkYesod "App" [parseRoutes|
/codegen HomeR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App where
    makeLogger = \_app -> myLogger
    maximumContentLength = \_app -> (\_anyRouteReally -> Just 256000000)
    messageLoggerSource _app = messageLoggerWithoutSource

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

postHomeR :: Handler Value
postHomeR = do
    ast <- requireCheckJsonBody :: Handler Ast.Root
    let result = codeGen ast
    let numCallables = Prelude.length (actualCallables result)
    let receivedFilename = Ast.filename ast
    let message = "receivedAst=" ++ receivedFilename ++ ", numCallables=" ++ show numCallables
    $logInfoS "(CodeGen)" (T.pack message)
    returnJson result

main :: IO ()
main = do
    waiApp <- toWaiAppPlain App
    run 3000 $ defaultMiddlewaresNoLogging waiApp
