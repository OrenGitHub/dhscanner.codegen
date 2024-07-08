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

-- project imports
import Asts
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

instance Yesod App where maximumContentLength = \app -> (\anyRouteReally -> Just 8000000)

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

postHomeR :: Handler Value
postHomeR = do
    _asts <- requireCheckJsonBody :: Handler Asts
    returnJson $ codeGen _asts

main :: IO ()
main = warp 3000 App
