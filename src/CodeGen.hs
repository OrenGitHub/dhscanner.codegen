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

-- |
-- * Monadic operation
--
-- * adds declared variable to symbol table
--
-- * Generating the initial value (if it exists) is non monadic
--
codeGenDecVar :: Ast.DecVarContent -> CodeGenContext Cfg
codeGenDecVar d = case Ast.decVarInitValue d of
    Nothing -> codeGenDecVarNoInit d (Token.getVarNameLocation (Ast.decVarName d))
    Just init -> codeGenDecVarInit (Ast.decVarName d) (Ast.decVarNominalType d) init

-- |
-- * update the symbol table with the declared variable
--
-- * return an empty cfg (nop) since nothing happens (no init)
codeGenDecVarNoInit :: Ast.DecVarContent -> Location -> CodeGenContext Cfg
codeGenDecVarNoInit d loc = do { ctx <- get; put (codeGenDecVarNoInit' d ctx); return $ Cfg.empty loc }

-- |
-- * update the symbol table with the declared variable
--
-- * generate the code for the init value
--
-- * append an assign instruction for the init value
--
-- * return the resulted cfg
--
codeGenDecVarInit :: Token.VarName -> Token.NominalTy -> Ast.Exp -> CodeGenContext Cfg
codeGenDecVarInit varName nominalType init = do
    ctx <- get; let { (cfg, t, actualType) = codeGenExp init ctx; fqn = Fqn.convertFrom actualType }
    put $ codeGenDecVarInit' varName init ctx; return $ codeGenDecVarInit'' varName fqn (cfg, t)

-- | pure non-monadic function, just update
-- the symbol table with the declared variable
codeGenDecVarNoInit' :: Ast.DecVarContent -> CodeGenState -> CodeGenState
codeGenDecVarNoInit' decVar ctx = let
    varName = Token.getVarNameToken (Ast.decVarName decVar)
    nominalType = Token.getNominalTyToken (Ast.decVarNominalType decVar) 
    actualType = SymbolTable.lookup nominalType (symbolTable ctx)
    symbolTable' = SymbolTable.insert varName actualType (symbolTable ctx)
    in ctx { symbolTable = symbolTable' }

-- | Non monadic helper function for updating the symbol table
codeGenDecVarInit' :: Token.VarName -> Ast.Exp -> CodeGenState -> CodeGenState
codeGenDecVarInit' varName initValue ctx = let
    (cfg, tmpVariable, actualType) = codeGenExp initValue ctx
    symbolTable' = SymbolTable.insertVarName varName actualType (symbolTable ctx)
    in ctx { symbolTable = symbolTable' }

-- | Non monadic helper function for creating the cfg of the init value
codeGenDecVarInit'' :: Token.VarName -> Fqn -> (Cfg, Bitcode.TmpVariable) -> Cfg
codeGenDecVarInit'' varName fqn (cfg, tmpVariable) = let
    srcVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    assignContent = Bitcode.AssignContent { assignOutput = srcVariable, assignInput = tmpVariable }
    assign = Bitcode.Assign assignContent
    location' = Token.getVarNameLocation varName
    instruction = Bitcode.Instruction { Bitcode.location = location', instructionContent = assign }
    in cfg `Cfg.concat` (Cfg.atom (Node instruction))

-- | helper non monadic function
returnType'' :: ActualType -> ActualType
returnType'' (ActualType.Function f) = ActualType.returnType f
returnType'' _ = ActualType.Any

-- | helper non monadic function
returnType' :: Token.Named -> SymbolTable -> ActualType
returnType' = fmap returnType'' . SymbolTable.lookup

-- | helper non monadic function
returnType :: Ast.DecFuncContent -> CodeGenState -> ActualType.ActualType
returnType d ctx = returnType' ((Token.getFuncNameToken . Ast.decFuncName) d) (symbolTable ctx)

-- | helper non monadic function for return value
returnedValue :: Ast.DecFuncContent -> CodeGenState -> Bitcode.TmpVariable
returnedValue d ctx = Bitcode.TmpVariable (Fqn.convertFrom (returnType d ctx)) (Ast.decFuncLocation d)

-- | helper non monadic function for return value
assemble :: Location -> Bitcode.InstructionContent -> Cfg
assemble = fmap (Cfg.atom . Node) . Bitcode.Instruction

-- | helper non monadic function for return instruction (as an atom cfg)
singleReturnSite :: Bitcode.TmpVariable -> Cfg
singleReturnSite = (uncurry assemble) . (Bitcode.tmpVariableLocation &&& (Bitcode.Return . Bitcode.ReturnContent . Just))

instrumentReturn :: Ast.DecFuncContent -> CodeGenContext ()
instrumentReturn decFunc = do { ctx <- get;
    put $ let r = returnedValue decFunc ctx in CodeGenState {
        symbolTable = symbolTable ctx,
        returnValue = Just r,
        returnTo = Just (singleReturnSite r),
        continueTo = continueTo ctx,
        breakTo = breakTo ctx
    }
}

cleanReturnInstrumentation :: CodeGenState -> CodeGenState
cleanReturnInstrumentation ctx = ctx { returnValue = Nothing, returnTo = Nothing } 

-- |
-- * create the prologue + body and concatenate them
--
-- * symbol table is /unchanged/ since the function's signature
-- already exists in it
--
codeGenDecFunc :: Ast.DecFuncContent -> CodeGenContext Cfg
codeGenDecFunc decFunc = do { ctx <- get;
    prologue <- codeGenDecFuncBody decFunc;
    instrumentReturn decFunc;
    body <- codeGenDecFuncBody decFunc;
    put $ cleanReturnInstrumentation ctx;
    return $ prologue `Cfg.concat` body
}

codeGenDecFuncBody :: Ast.DecFuncContent -> CodeGenContext Cfg
codeGenDecFuncBody decFunc = codeGenStmts (Ast.decFuncBody decFunc) (Ast.decFuncLocation decFunc)

codeGenStmts :: [ Ast.Stmt ] -> Location -> CodeGenContext Cfg 
codeGenStmts stmts l = do { cfgs <- codeGenStmts' stmts; return $ foldl' Cfg.concat (Cfg.empty l) cfgs }

codeGenStmts' :: [ Ast.Stmt ] -> CodeGenContext [ Cfg ]
codeGenStmts' = mapM codeGenStmt

codeGenStmt :: Ast.Stmt -> CodeGenContext Cfg
codeGenStmt (Ast.StmtWhile stmtWhile) = codeGenStmtWhile stmtWhile
codeGenStmt (Ast.StmtReturn stmtReturn) = do { ctx <- get; return $ codeGenStmtReturn stmtReturn ctx }
codeGenStmt _ = undefined

instrumentLoopHeader :: Ast.StmtWhileContent -> Cfg
instrumentLoopHeader stmtWhile = undefined

instrumentLoopExit:: Ast.StmtWhileContent -> Cfg
instrumentLoopExit stmtWhile = undefined

instrumentWhileLoop :: Ast.StmtWhileContent -> CodeGenState -> CodeGenState
instrumentWhileLoop stmtWhile ctx = let
     loopHeader = instrumentLoopHeader stmtWhile
     loopExit = instrumentLoopExit stmtWhile
     in ctx { continueTo = Just loopHeader, breakTo = Just loopExit }

cleanWhileLoopInstrumentation :: CodeGenState -> CodeGenState
cleanWhileLoopInstrumentation ctx = ctx { breakTo = Nothing, continueTo = Nothing } 

-- | trivial monadic helper functions
codeGenStmtWhileCond :: Ast.StmtWhileContent -> CodeGenState -> (Cfg,Bitcode.TmpVariable,ActualType)
codeGenStmtWhileCond stmtWhile ctx = codeGenExp (Ast.stmtWhileCond stmtWhile) ctx

-- | trivial monadic helper functions
codeGenStmtWhileBody :: Ast.StmtWhileContent -> CodeGenContext Cfg
codeGenStmtWhileBody = uncurry codeGenStmts . (Ast.stmtWhileBody &&& Ast.stmtWhileLocation)

-- | code generation for while loops
codeGenStmtWhile :: Ast.StmtWhileContent -> CodeGenContext Cfg
codeGenStmtWhile stmtWhile = do
    ctx <- get;
    let (cfgCond, tmpVariableCond, actualType) = codeGenStmtWhileCond stmtWhile ctx;
    put $ instrumentWhileLoop stmtWhile ctx;
    cfgBody <- codeGenStmtWhileBody stmtWhile;
    put $ cleanWhileLoopInstrumentation ctx;
    return $ Cfg.loopify cfgCond cfgBody tmpVariableCond

-- | Non monadic helper function
codeGenStmtReturnValue' :: Ast.Exp -> Cfg -> Bitcode.TmpVariable -> Cfg
codeGenStmtReturnValue' value cfg tmpVariable = let
    location = Bitcode.tmpVariableLocation tmpVariable
    bitcodeReturn = Bitcode.Return $ Bitcode.ReturnContent (Just tmpVariable)
    instruction = Bitcode.Instruction location bitcodeReturn
    in cfg `Cfg.concat` (Cfg.atom (Node instruction))

codeGenStmtReturnValue :: Ast.Exp -> CodeGenState -> Cfg
codeGenStmtReturnValue value ctx = let
    (cfg, tmpVariable, actualType) = codeGenExp value ctx
    in codeGenStmtReturnValue' value cfg tmpVariable

-- |
codeGenStmtReturnNoValue :: Ast.StmtReturnContent -> CodeGenState -> Cfg
codeGenStmtReturnNoValue returnStmt ctx = let
    location = Ast.stmtReturnLocation returnStmt
    bitcodeReturn = Bitcode.Return $ Bitcode.ReturnContent Nothing
    instruction = Bitcode.Instruction location bitcodeReturn
    in Cfg.atom (Node instruction)

-- | Pseudo code for return statements:
-- @
--     return 88;
--
--     Temp_749 := 88
--     Temp_155 := Temp_749 --- cfg edge --> return Temp_155
--     Assume False
--
-- @
--
codeGenStmtReturn :: Ast.StmtReturnContent -> CodeGenState -> Cfg
codeGenStmtReturn stmtReturn ctx = case Ast.stmtReturnValue stmtReturn of
    Nothing -> codeGenStmtReturnNoValue stmtReturn ctx
    Just value -> codeGenStmtReturnValue value ctx

codeGenExp :: Ast.Exp -> CodeGenState -> (Cfg, Bitcode.TmpVariable, ActualType)
codeGenExp (Ast.ExpInt   expInt  ) _ = codeGenExpInt expInt
codeGenExp (Ast.ExpStr   expStr  ) _ = codeGenExpStr expStr
codeGenExp (Ast.ExpVar   expVar  ) _ = undefined
codeGenExp (Ast.ExpCall  expCall ) _ = undefined
codeGenExp (Ast.ExpBinop expBinop) _ = undefined

codeGenExpInt :: Ast.ExpIntContent -> (Cfg, Bitcode.TmpVariable, ActualType)
codeGenExpInt expInt = let
    constInt = Ast.expIntValue expInt
    location = Ast.locationExpInt expInt
    tmpVariable = Bitcode.TmpVariable Fqn.nativeInt location
    loadImmInt = Bitcode.LoadImmContentInt tmpVariable constInt
    instruction = Bitcode.Instruction location $ Bitcode.LoadImm loadImmInt
    in (Cfg.atom $ Node instruction, tmpVariable, ActualType.nativeTypeInt)

codeGenExpStr :: Ast.ExpStrContent -> (Cfg, Bitcode.TmpVariable, ActualType)
codeGenExpStr expStr = let
    constStr = Ast.expStrValue expStr
    location = Ast.locationExpStr expStr
    tmpVariable = Bitcode.TmpVariable Fqn.nativeStr location
    loadImmStr = Bitcode.LoadImmContentStr tmpVariable constStr
    instruction = Bitcode.Instruction location $ Bitcode.LoadImm loadImmStr
    in (Cfg.atom $ Node instruction, tmpVariable, ActualType.nativeTypeStr)

