module SymbolTable

where

-- project imports
import ActualType
import qualified Token
import qualified Bitcode

-- general imports
import Prelude hiding (lookup)
import Data.Map ( Map )
import Data.Maybe

-- general (qualified) imports
import qualified Data.Map

-- | an /internal/ data type to accommodate
-- a variables's actual type, fqn, and bitcode temporary
data Variable
   = Variable
     {
         actualType :: ActualType,
         bitcodeVar :: Bitcode.Variable
     }
     deriving ( Show )

-- | Unexposed wrapper around Data.Map
data Scope = Scope { actualScope :: Map String Variable } deriving ( Show )

-- |
-- * A stack-like data structure to manage scopes while traversing the AST
--
-- * Note that the scopes stack is /never/ empty
--
-- * the scopes start from the current (innermost) scope
--
data SymbolTable = SymbolTable { scopes :: [ Scope ] } deriving ( Show )

-- | Unexposed, internal use only
newEmptyScope :: Scope
newEmptyScope = Scope { actualScope = Data.Map.empty }

-- | It is ther user's responsibility that `beginScope` is coupled with its `endScope`
beginScope :: SymbolTable -> SymbolTable
beginScope = SymbolTable . (newEmptyScope:) . scopes

-- | It is ther user's responsibility that `beginScope` is coupled with its `endScope`
endScope :: SymbolTable -> SymbolTable
endScope = SymbolTable . tail . scopes

emptySymbolTable = SymbolTable { scopes = [] }

insert :: Token.Named -> Bitcode.Variable -> ActualType -> SymbolTable -> SymbolTable
insert name bitcodeVar actualType symbolTable = let
    currentScope = actualScope (head (scopes symbolTable))
    currentScope' = Scope $ Data.Map.insert (Token.content name) (Variable actualType bitcodeVar) currentScope
    in SymbolTable { scopes = currentScope' : (tail (scopes symbolTable)) }

varExists' :: String -> [ Scope ] -> Bool
varExists' s scopes = isJust (lookup' s scopes)

-- | 
varExists :: Token.VarName -> SymbolTable -> Bool
varExists v table = varExists' (Token.content (Token.getVarNameToken v)) (scopes table)

insertVar:: Token.VarName -> Bitcode.Variable -> ActualType -> SymbolTable -> SymbolTable
insertVar name v t = insert (Token.getVarNameToken name) v t

lookupActualType :: Token.Named -> SymbolTable -> ActualType
lookupActualType name table = case lookup' (Token.content name) (scopes table) of
    Nothing -> ActualType.Any
    Just variable -> (actualType variable)

lookupBitcodeVar :: Token.Named -> SymbolTable -> Maybe Bitcode.Variable
lookupBitcodeVar name table = case lookup' (Token.content name) (scopes table) of
    Nothing -> Nothing
    Just v -> Just (bitcodeVar v)


-- Internal (recursive) lookup
-- The scopes list starts with the /innermost/ scope
lookup' :: String -> [ Scope ] -> Maybe Variable
lookup' _ [] = Nothing
lookup' name (scope:outerScopes) = case Data.Map.lookup name (actualScope scope) of
    Nothing -> lookup' name outerScopes
    Just variable -> Just variable

