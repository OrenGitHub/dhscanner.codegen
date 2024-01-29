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

-- project imports
import Asts
import CodeGen ( codeGen )

data App = App

mkYesod "App" [parseRoutes|
/ HomeR POST
|]

instance Yesod App

postHomeR :: Handler Value
postHomeR = do
    asts <- requireCheckJsonBody :: Handler Asts
    returnJson $ codeGen asts

main :: IO ()
main = warp 3000 App
