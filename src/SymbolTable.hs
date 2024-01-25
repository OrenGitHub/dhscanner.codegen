module SymbolTable

where

import Prelude hiding (lookup)
import qualified Data.Map as Map

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
     deriving ( Show )

newEmptyScope = Scope { actualScope = Map.empty }

beginScope :: SymbolTable -> SymbolTable
beginScope table = SymbolTable { scopes = newEmptyScope:(scopes table) }

endScope :: SymbolTable -> SymbolTable
endScope table = SymbolTable { scopes = tail $ scopes table }

lookup :: Token.Named -> SymbolTable -> Maybe ActualType
lookup name table = lookup' (Token.content name) (scopes table)

-- Internal (recursive) lookup
-- The scopes list starts with the /innermost/ scope
lookup' :: String -> [ Scope ] -> Maybe ActualType
lookup' _ [] = Nothing
lookup' name (scope:outerScopes) = case Map.lookup scope of
    Nothing -> lookup' name outerScopes
    Just actualType -> actualType
