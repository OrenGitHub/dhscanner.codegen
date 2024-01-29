module CodeGen

where

import Asts
import qualified Ast
import Callable

codeGen :: Asts -> Callables
codeGen asts = Callables { actualCallables = [] }
