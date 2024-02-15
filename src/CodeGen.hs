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
import ActualType
import Bitcode (
    assignInput,
    assignOutput,
    location,
    actualType,
    instructionContent)
import SymbolTable (
    SymbolTable,
    emptySymbolTable)

-- project (qualified) imports
import qualified Ast
import qualified Token
import qualified Bitcode
import qualified SymbolTable

-- general imports
import Control.Monad.State.Lazy
import Control.Monad ( foldM )

data CodeGenState
   = CodeGenState
     {
         symbolTable :: SymbolTable
     }
     deriving ( Show )

type CodeGenContext = State CodeGenState

initCodeGenState = CodeGenState emptySymbolTable

codeGen :: Asts -> [[ Cfg ]]
codeGen asts = evalState (codeGenRoots (astsContent asts)) initCodeGenState

codeGenRoots :: [ Ast.Root ] -> CodeGenContext [[ Cfg ]]
codeGenRoots = mapM codeGenRoot 

codeGenRoot :: Ast.Root -> CodeGenContext [ Cfg ]
codeGenRoot = codeGenDecs . Ast.actualAst

codeGenDecs :: [ Ast.Dec ] -> CodeGenContext [ Cfg ]
codeGenDecs = mapM codeGenDec 

codeGenDec :: Ast.Dec -> CodeGenContext Cfg
codeGenDec (Ast.DecVar    decVar   ) = codeGenDecVar    decVar
codeGenDec (Ast.DecFunc   decFunc  ) = codeGenDecFunc   decFunc
codeGenDec (Ast.DecClass  decClass ) = undefined
codeGenDec (Ast.DecMethod decMethod) = undefined
codeGenDec (Ast.DecImport decImport) = undefined

codeGenDecVar :: Ast.DecVarContent -> CodeGenContext Cfg
codeGenDecVar decVar = case Ast.decVarInitValue decVar of
    Nothing -> codeGenDecVarNoInit decVar
    Just initValue -> let
        varName = Ast.decVarName decVar
        nominalType = Ast.decVarNominalType decVar
        in codeGenDecVarInitialized varName nominalType initValue

-- | update the symbol table with the declared variable
codeGenDecVarNoInit' :: Ast.DecVarContent -> CodeGenState -> CodeGenState
codeGenDecVarNoInit' decVar ctx = let
    varName = Token.getVarNameToken (Ast.decVarName decVar)
    nominalType = Token.getNominalTyToken (Ast.decVarNominalType decVar) 
    actualType = SymbolTable.lookup nominalType (symbolTable ctx)
    symbolTable' = SymbolTable.insert varName actualType (symbolTable ctx)
    in CodeGenState { symbolTable = symbolTable' }

-- |
-- * update the symbol table with the declared variable
--
-- * return an empty cfg (nop) since nothing happens (no init)
codeGenDecVarNoInit :: Ast.DecVarContent -> CodeGenContext Cfg
codeGenDecVarNoInit decVar = do {
    ctx <- get; put (codeGenDecVarNoInit' decVar ctx)
    return $ Cfg.empty (Token.getVarNameLocation (Ast.decVarName decVar))
}

-- | Non monadic helper function
codeGenDecVarInitialized' :: Token.VarName -> ActualType -> CodeGenState -> CodeGenState
codeGenDecVarInitialized' varName actualType ctx = let
    symbolTable' = SymbolTable.insert (Token.getVarNameToken varName) actualType (symbolTable ctx)
    in CodeGenState { symbolTable = symbolTable' }

-- | Non monadic helper function
codeGenDecVarInitialized'' :: Token.VarName -> (Bitcode.TmpVariable, Cfg) -> Cfg
codeGenDecVarInitialized'' varName (tmpVariable, cfg) = let
    srcVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable varName
    assignContent = Bitcode.AssignContent { assignOutput = srcVariable, assignInput = tmpVariable }
    assign = Bitcode.Assign assignContent
    location' = Token.getVarNameLocation varName
    instruction = Bitcode.Instruction { Bitcode.location = location', instructionContent = assign }
    in cfg `Cfg.concat` (Cfg.atom (Node instruction))

-- |
-- * update the symbol table with the declared variable
--
-- * generate the code for the init value
--
-- * append an assign instruction for the init value
--
-- * return the resulted cfg
codeGenDecVarInitialized :: Token.VarName -> Token.NominalTy -> Ast.Exp -> CodeGenContext Cfg
codeGenDecVarInitialized varName nominalType initValue = do {
    ctx <- get; put (codeGenDecVarInitialized' varName (actualType (fst (codeGenExp initValue))) ctx)
    return $ codeGenDecVarInitialized'' varName (codeGenExp initValue)
}

codeGenDecFunc :: Ast.DecFuncContent -> CodeGenContext Cfg
codeGenDecFunc decFunc = codeGenStmts (Ast.decFuncBody decFunc)

codeGenStmts :: [ Ast.Stmt ] -> CodeGenContext Cfg
codeGenStmts stmts = foldM Cfg.empty `Cfg.concat` (mapM codeGenStmt stmts)

codeGenStmt :: Ast.Stmt -> CodeGenContext Cfg
codeGenStmt (Ast.StmtWhile stmtWhile) = codeGenStmtWhile stmtWhile


codeGenStmtWhile :: Ast.StmtWhileContent -> CodeGenContext Cfg
codeGenStmtWhile stmtWhile = Cfg.loopify cond body guard location
    where
        cond = codeGenExp (Ast.stmtWhileCond stmtWhile)
        body = codeGenStmts (Ast.stmtWhileBody stmtWhile)
        guard = codeGenExp (Ast.stmtWhileCond stmtWhile)
        location = Ast.stmtWhileLocation stmtWhile

codeGenStmtReturn :: Ast.StmtReturnContent -> CodeGenContext Cfg
codeGenStmtReturn stmtReturn = case Ast.stmtReturnValue stmtReturn of
    Nothing -> Cfg.atom (Cfg.Node (Bitcode.Instruction (Ast.stmtReturnLocation stmtReturn) Bitcode.Return))
    Just returnValue -> let (_, cfg) = codeGenExp returnValue in cfg

codeGenExp :: Ast.Exp -> CodeGenContext (Cfg, Bitcode.TmpVariable)
codeGenExp (Ast.ExpInt   expInt  ) = codeGenExpInt   expInt
codeGenExp (Ast.ExpStr   expStr  ) = undefined
codeGenExp (Ast.ExpVar   expVar  ) = undefined
codeGenExp (Ast.ExpCall  expCall ) = undefined
codeGenExp (Ast.ExpBinop expBinop) = undefined

mkFreshIntTmp :: Location -> Bitcode.TmpVariable
mkFreshIntTmp l = Bitcode.TmpVariable { actualType = ActualType.nativeTypeInt, Bitcode.tmpVariableLocation = l }

codeGenExpInt :: Ast.ExpIntContent -> CodeGenContext (Cfg, Bitcode.TmpVariable)
codeGenExpInt expIntContent = return $ let
    constInt = Ast.expIntValue expIntContent
    location = Token.constIntLocation constInt
    tmpVariable = mkFreshIntTmp location
    loadImmInt = Bitcode.LoadImmContentInt tmpVariable constInt
    loadImm = Bitcode.LoadImm loadImmInt
    in (Cfg.atom (Node (Bitcode.Instruction location loadImm)), tmpVariable)

