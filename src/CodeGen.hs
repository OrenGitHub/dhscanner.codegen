module CodeGen

where

import Asts
import qualified Ast
import Control.Monad.State

codeGenDecClass :: Package -> Ast.DecClass -> Int
codeGenDecClass astDecClass = 

codeGenDec :: Ast.Dec -> Int
codeGenDec (Ast.DecFuncCtor astDecFunc) = codeGenDecFunc astDecFunc
codeGenDec (Ast.DecClassCtor astDecClass) = codeGenDecClass astDecClass

codeGen :: Asts -> Callables
codeGen asts = let
    packages = packager asts
    packages' = typify asts packages
    callables = codeGen' asts packages'
