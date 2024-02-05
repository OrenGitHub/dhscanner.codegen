module CodeGen

where

import Data.Set ( fromList )
import Data.Maybe ( catMaybes )

-- project imports
import Fqn
import Cfg
import Asts
import Callable
import Location
import qualified Ast
import qualified Token
import qualified Bitcode

codeGen :: Asts -> Callables
codeGen asts = Callables { actualCallables = codeGen'''' (astsContent asts) }

codeGen'''' :: [ Ast.Root ] -> [ Callable ]
codeGen'''' roots = catMaybes $ map codeGen''' roots

codeGen''' :: Ast.Root -> Maybe Callable
codeGen''' root = codeGen'' (Ast.actualAst root)

codeGen'' :: [ Ast.Dec ] -> Maybe Callable
codeGen'' decs = codeGen' $ filter keepJustDecVar decs

codeGen' :: [ Ast.Dec ] -> Maybe Callable
codeGen' ((Ast.DecVarCtor d):_) = Just $ codeGenDecVar d
codeGen' _ = Nothing

keepJustDecVar :: Ast.Dec -> Bool
keepJustDecVar (Ast.DecVarCtor _) = True
keepJustDecVar _ = False

codeGenDecVar :: Ast.DecVar -> Callable
codeGenDecVar d = Callable { cfg = cfgDecVar d, fqn = Fqn { content = "" } }

mkNopInstruction :: Location -> Node
mkNopInstruction l = Node { theInstructionInside = Bitcode.mkNopInstruction l }

cfgDecVar :: Ast.DecVar -> Cfg
cfgDecVar d = Cfg { entry = entry', exit = exit', location = location', edges = edges' }
    where
        location' = Token.getVarNameLocation $ Ast.decVarName d
        entry' = mkNopInstruction location'
        exit'  = mkNopInstruction location'
        edges' = Edges { actualEdges = fromList [ Edge { from = entry', to = exit' } ] }
 
