{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Prelude
import Data.Aeson()
import GHC.Generics
import Data.Text.Lazy
import Data.Aeson.Text (encodeToLazyText)

-- project imports
import Ast ( Asts )

data App = App

mkYesod "App" [parseRoutes|
/ HomeR POST
|]

instance Yesod App

postHomeR :: Handler Value
postHomeR = do
    asts <- requireCheckJsonBody :: Handler Asts
    returnJson (encodeToLazyText asts)

main :: IO ()
main = warp 3000 App
