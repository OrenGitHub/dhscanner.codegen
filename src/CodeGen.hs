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
import ActualType hiding ( returnType )
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
import qualified ActualType
import qualified SymbolTable

-- general imports
import Data.List
import Control.Arrow
import Control.Monad.State.Lazy
import Control.Monad ( foldM )

data CodeGenState
   = CodeGenState
     {
         symbolTable :: SymbolTable,
         returnValue :: Maybe Bitcode.TmpVariable,
         returnTo :: Maybe Cfg,
         continueTo :: Maybe Cfg,
         breakTo :: Maybe Cfg
     }
     deriving ( Show )

type CodeGenContext = State CodeGenState

initCodeGenState = CodeGenState {
    symbolTable = emptySymbolTable,
    returnValue = Nothing,
    returnTo = Nothing,
    continueTo = Nothing,
    breakTo = Nothing
}

-- | API (non monadic)
codeGen :: Asts -> [ Cfg ]
codeGen asts = Data.List.foldl' (++) [] (codeGen' asts)

-- | Launch monadic computations from initial code gen state
codeGen' :: Asts -> [[ Cfg ]]
codeGen' asts = evalState (codeGenRoots (astsContent asts)) initCodeGenState

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

-- | split depending on the existence of init value
codeGenDecVar :: Ast.DecVarContent -> CodeGenContext Cfg
codeGenDecVar decVar = case Ast.decVarInitValue decVar of
    Nothing -> codeGenDecVarNoInit decVar
    Just initValue -> let
        varName = Ast.decVarName decVar
        nominalType = Ast.decVarNominalType decVar
        in codeGenDecVarInitialized varName nominalType initValue

-- | pure non-monadic function, just update
-- the symbol table with the declared variable
codeGenDecVarNoInit' :: Ast.DecVarContent -> CodeGenState -> CodeGenState
codeGenDecVarNoInit' decVar ctx = let
    varName = Token.getVarNameToken (Ast.decVarName decVar)
    nominalType = Token.getNominalTyToken (Ast.decVarNominalType decVar) 
    actualType = SymbolTable.lookup nominalType (symbolTable ctx)
    symbolTable' = SymbolTable.insert varName actualType (symbolTable ctx)
    in ctx { symbolTable = symbolTable' }

-- |
-- * update the symbol table with the declared variable
--
-- * return an empty cfg (nop) since nothing happens (no init)
codeGenDecVarNoInit :: Ast.DecVarContent -> CodeGenContext Cfg
codeGenDecVarNoInit decVar = do {
    ctx <- get; put (codeGenDecVarNoInit' decVar ctx);
    return $ Cfg.empty (Token.getVarNameLocation (Ast.decVarName decVar))
}

-- | Non monadic helper function for updating the symbol table
codeGenDecVarInitialized' :: Token.VarName -> ActualType -> CodeGenState -> CodeGenState
codeGenDecVarInitialized' varName actualType ctx = let
    symbolTable' = SymbolTable.insert (Token.getVarNameToken varName) actualType (symbolTable ctx)
    in ctx { symbolTable = symbolTable' }

-- | Non monadic helper function for creating the cfg of the init value
codeGenDecVarInitialized'' :: Token.VarName -> (Cfg, Bitcode.TmpVariable) -> Cfg
codeGenDecVarInitialized'' varName (cfg, tmpVariable) = let
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
    (cfg, tmpVariable) <- codeGenExp initValue; ctx <- get;
    put (codeGenDecVarInitialized' varName (Bitcode.actualType tmpVariable) ctx);
    return $ codeGenDecVarInitialized'' varName (cfg, tmpVariable)
}

-- | helper non monadic function
returnType'' :: ActualType -> ActualType
returnType'' (ActualType.Function f) = ActualType.returnType f
returnType'' _ = ActualType.Any

-- | helper non monadic function
returnType' :: Token.Named -> SymbolTable -> ActualType
returnType' = returnType'' . SymbolTable.lookup

-- | helper non monadic function
returnType :: Ast.DecFuncContent -> CodeGenState -> ActualType
returnType = returnType' (Token.getFuncNameToken . Ast.decFuncName *** symbolTable)

-- | helper non monadic function for return value
returnedValue :: Ast.DecFuncContent -> CodeGenState -> Bitcode.TmpVariable
returnedValue d ctx = Bitcode.TmpVariable (Ast.locationDec d) (returnType d ctx)

-- | helper non monadic function for return instruction (as an atom cfg)
singleReturnSite :: Ast.DecFuncContent -> CodeGenState -> Cfg
singleReturnSite tmpVariable location = let
    ret = Bitcode.Return $ Bitcode.ReturnContent (Just tmpVariable)
    in Cfg.atom $ Node { theInstructionInside = Bitcode.Instruction location ret }

instrumentReturn :: Ast.DecFuncContent -> CodeGenState -> CodeGenState
instrumentReturn decFunc ctx = let
    returnedValue' = returnedValue decFunc ctx
    returnSite = singleReturnSite decFunc ctx
    in ctx { returnValue = returnedValue', returnTo = returnSite }

-- |
-- * create the prologue + body and concatenate them
--
-- * symbol table is /unchanged/ since the function's signature
-- already exists in it
--
codeGenDecFunc :: Ast.DecFuncContent -> CodeGenContext Cfg
codeGenDecFunc decFunc = do { ctx <- get;
    prologue <- codeGenDecFuncBody decFunc;
    put $ instrumentReturn decFunc ctx;
    body <- codeGenDecFuncBody decFunc;
    put $ cleanReturnInstrumentation ctx;
    return $ prologue `Cfg.concat` body `Cfg.concat` (ret'' decFunc ctx)
}

codeGenDecFuncBody :: Ast.DecFuncContent -> CodeGenContext Cfg
codeGenDecFuncBody decFunc = codeGenStmts (Ast.decFuncBody decFunc) (Ast.decFuncLocation decFunc)

codeGenStmts :: [ Ast.Stmt ] -> Location -> CodeGenContext Cfg 
codeGenStmts stmts l = do { cfgs <- codeGenStmts' stmts; return $ foldl' Cfg.concat (Cfg.empty l) cfgs }

codeGenStmts' :: [ Ast.Stmt ] -> CodeGenContext [ Cfg ]
codeGenStmts' = mapM codeGenStmt

codeGenStmt :: Ast.Stmt -> CodeGenContext Cfg
codeGenStmt (Ast.StmtWhile stmtWhile) = codeGenStmtWhile stmtWhile
codeGenStmt _ = undefined

instrumentWhileLoop :: Ast.StmtWhileContent -> CodeGenState -> CodeGenState
instrumentWhileLoop stmtWhile ctx = let
     loopHeader = instrumentLoopHeader
     loopExit = instrumentLoopExit
     in ctx { continueTo = loopHeader, breakTo = loopExit }

-- | trivial monadic helper functions
codeGenStmtWhileCond :: Ast.StmtWhileContent -> CodeGenContext (Bitcode.TmpVariable,Cfg)
codeGenStmtWhileCond = codeGenExp . Ast.stmtWhileCond

-- | trivial monadic helper functions
codeGenStmtWhileBody :: Ast.StmtWhileContent -> CodeGenContext Cfg
codeGenStmtWhileBody = uncurry codeGenStmts (Ast.stmtWhileBody &&& Ast.stmtWhileLocation)

-- | code generation for while loops
codeGenStmtWhile :: Ast.StmtWhileContent -> CodeGenContext Cfg
codeGenStmtWhile stmtWhile = do { ctx <- get;
    (cfgCond, tmpVariableCond) <- codeGenStmtWhileCond stmtWhile;
    put $ instrumentWhileLoop stmtWhile ctx;
    cfgBody <- codeGenStmtWhileBody stmtWhile;
    put $ cleanWhileLoopInstrumentation ctx;
    return $ Cfg.loopify cfgCond cfgBody tmpVariableCond
}

-- | Non monadic helper function
codeGenStmtReturnValue' :: Ast.Exp -> Cfg -> Bitcode.TmpVariable -> Cfg
codeGenStmtReturnValue' value cfg tmpVariable = let
    location = Bitcode.tmpVariableLocation tmpVariable
    bitcodeReturn = Bitcode.Return $ Bitcode.ReturnContent (Just tmpVariable)
    instruction = Bitcode.Instruction location bitcodeReturn
    in cfg `Cfg.concat` (Cfg.atom (Node instruction))

codeGenStmtReturnValue :: Ast.Exp -> CodeGenContext Cfg
codeGenStmtReturnValue value = do {
    (cfg, tmpVariable) <- codeGenExp value;
    return $ codeGenStmtReturnValue' value cfg tmpVariable
}

codeGenStmtReturnNoValue :: Ast.StmtReturnContent -> CodeGenContext Cfg
codeGenStmtReturnNoValue returnStmt = return $ let
    location = Ast.stmtReturnLocation returnStmt
    bitcodeReturn = Bitcode.Return $ Bitcode.ReturnContent Nothing
    instruction = Bitcode.Instruction location bitcodeReturn
    in Cfg.atom (Node instruction)

codeGenStmtReturn :: Ast.StmtReturnContent -> CodeGenContext Cfg
codeGenStmtReturn stmtReturn = case Ast.stmtReturnValue stmtReturn of
    Nothing -> codeGenStmtReturnNoValue stmtReturn
    Just value -> codeGenStmtReturnValue value

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

