module SymbolTable

where

-- project imports
import ActualType
import qualified Token

-- general imports
import Prelude hiding (lookup)
import Data.Map ( Map )

-- general (qualified) imports
import qualified Data.Map

data SymbolTable
   = SymbolTable
     {
         scopes :: [ Scope ]
     }
     deriving ( Show, Eq )

data Scope
   = Scope
     {
         actualScope :: Map String ActualType
     }
     deriving ( Show, Eq )

newEmptyScope :: Scope
newEmptyScope = Scope { actualScope = Data.Map.empty }

beginScope :: SymbolTable -> SymbolTable
beginScope table = SymbolTable { scopes = newEmptyScope:(scopes table) }

endScope :: SymbolTable -> SymbolTable
endScope table = SymbolTable { scopes = tail $ scopes table }

emptySymbolTable = SymbolTable { scopes = [] }

insert :: Token.Named -> ActualType -> SymbolTable -> SymbolTable
insert name actualType symbolTable = let
    currentScope = actualScope (head (scopes symbolTable))
    currentScope' = Scope $ Data.Map.insert (Token.content name) actualType currentScope
    in SymbolTable { scopes = currentScope' : (tail (scopes symbolTable)) }

-- 
lookup :: Token.Named -> SymbolTable -> ActualType
lookup name table = case lookup' (Token.content name) (scopes table) of
    Nothing -> NativeType $ NativeTypeAny
    Just actualType -> actualType

-- Internal (recursive) lookup
-- The scopes list starts with the /innermost/ scope
lookup' :: String -> [ Scope ] -> Maybe ActualType
lookup' _ [] = Nothing
lookup' name (scope:outerScopes) = case Data.Map.lookup name (actualScope scope) of
    Nothing -> lookup' name outerScopes
    Just actualType -> Just actualType

