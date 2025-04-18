module SymbolTable

where

-- project imports
import Fqn
import Location
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
-- a variables's metadata. Note that the actual
-- is deliberately separated from the bitcode var
-- since it is needed only during codegen time, and
-- not needed in later phases: neither for abstract interpretation,
-- nor for knowledge base construction
-- Unexposed wrapper around Data.Map
data Scope = Scope { actualScope :: Map String (Bitcode.Variable,ActualType) } deriving ( Show )

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
endScope table = case scopes table of { (_:outerScopes) -> SymbolTable outerScopes; [] -> table }

createFstringFunc :: Token.FuncName -> ActualType
createFstringFunc f = let
    irrelevantParams = ActualType.Params []
    strRetType = ActualType.NativeTypeStr
    f' = ActualType.FunctionContent f irrelevantParams strRetType
    in ActualType.Function f'

createOsPython :: ActualType
createOsPython = ActualType.ThirdPartyImport $ ActualType.ThirdPartyImportContent "python.os"

createSubprocessPython :: ActualType
createSubprocessPython = ActualType.ThirdPartyImport $ ActualType.ThirdPartyImportContent "python.subprocess"

runtimeScope :: Scope
runtimeScope = let
    jsloc = Location "nodejs" 0 0 0 0 -- for native nodejs functions
    pyloc = Location "python" 0 0 0 0 -- for native python functions
    location' = Location "fstring" 0 0 0 0 -- instrumented format string function
    varNameRequire          = Token.VarName (Token.Named "require" jsloc)
    varNameFstring          = Token.VarName (Token.Named "fstring" location')
    varNameOsPython         = Token.VarName (Token.Named "python.os" pyloc)
    varNameSubprocessPython = Token.VarName (Token.Named "python.subprocess" pyloc)
    requireSpecialVar   = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn "nodejs.require")    varNameRequire
    fstringSpecialVar   = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn "fstring")           varNameFstring
    osPythonVar         = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn "python.os")         varNameOsPython
    subprocessPythonVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn "python.subprocess") varNameSubprocessPython
    fstringFuncName = Token.FuncName (Token.Named "fstring" location')
    fstringFunc = createFstringFunc fstringFuncName
    osPython = createOsPython
    subprocessPython = createSubprocessPython
    require    = ("require",    (requireSpecialVar,   ActualType.Require))
    fstring    = ("fstring",    (fstringSpecialVar,   fstringFunc))
    os         = ("os",         (osPythonVar,         osPython)) 
    subprocess = ("subprocess", (subprocessPythonVar, subprocessPython)) 
    in Scope $ Data.Map.fromList [ require, fstring, os, subprocess ]

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable { scopes = [ runtimeScope ] }

insert' :: String -> Bitcode.Variable -> ActualType -> Scope -> Scope
insert' content' b t (Scope s) = Scope $ Data.Map.insert content' (b,t) s

insert :: Token.Named -> Bitcode.Variable -> ActualType -> SymbolTable -> SymbolTable
insert _ _ _ (SymbolTable []) = SymbolTable [] -- unreachable
insert name bitcodeVar actualType (SymbolTable (scope:externalScopes)) = SymbolTable scopes'
    where scopes' = (insert' (Token.content name) bitcodeVar actualType scope):externalScopes

varExists' :: String -> [ Scope ] -> Bool
varExists' s scopes' = isJust (lookup' s scopes')

-- | 
varExists :: Token.VarName -> SymbolTable -> Bool
varExists v table = varExists' (Token.content (Token.getVarNameToken v)) (scopes table)

insertVar :: Token.VarName -> Bitcode.Variable -> ActualType -> SymbolTable -> SymbolTable
insertVar name v t = insert (Token.getVarNameToken name) v t

insertClass :: Token.ClassName -> Bitcode.Variable -> ActualType -> SymbolTable -> SymbolTable
insertClass name v t = insert (Token.getClassNameToken name) v t

insertParam:: Token.ParamName -> Bitcode.Variable -> ActualType -> SymbolTable -> SymbolTable
insertParam name v t = insert (Token.getParamNameToken name) v t

lookupVar :: Token.VarName -> SymbolTable -> Maybe (Bitcode.Variable, ActualType)
lookupVar v table = lookup' (Token.content $ Token.getVarNameToken v) (scopes table)

lookupSuperType :: Token.SuperName -> SymbolTable -> Maybe ActualType
lookupSuperType super table = case lookup' (Token.content $ Token.getSuperNameToken super) (scopes table) of
    Nothing -> Nothing
    Just (_, actualType) -> Just actualType

lookupNominalType :: Token.NominalTy -> SymbolTable -> ActualType
lookupNominalType t table = case lookup' (Token.content $ Token.getNominalTyToken t) (scopes table) of
    Nothing -> ActualType.Any
    Just (_, actualType) -> actualType

-- Internal (recursive) lookup
-- The scopes list starts with the /innermost/ scope
lookup' :: String -> [ Scope ] -> Maybe (Bitcode.Variable, ActualType)
lookup' _ [] = Nothing
lookup' name ((Scope scope):outerScopes) = case Data.Map.lookup name scope of
    Nothing -> lookup' name outerScopes
    Just variable -> Just variable

