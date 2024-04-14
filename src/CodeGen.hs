module CodeGen

where

-- project imports
import Fqn
import Cfg 
import Asts
import Location
import ActualType hiding ( returnType )
import Bitcode (
    assignInput,
    assignOutput,
    location,
    instructionContent)
import SymbolTable ( SymbolTable, emptySymbolTable)

-- project (qualified) imports
import qualified Ast
import qualified Token
import qualified Bitcode
import qualified ActualType
import qualified SymbolTable

-- general imports
import Data.Set ( fromList )
import Data.Maybe ( catMaybes )

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

-- |
-- * API function is /non/ monadic
--
-- * It launches monadic computations from initial code gen state
codeGen :: Asts -> [ Cfg ]
codeGen asts = evalState (codeGenRoots (astsContent asts)) initCodeGenState

codeGenRoots :: [ Ast.Root ] -> CodeGenContext [ Cfg ]
codeGenRoots = mapM codeGenRoot 

codeGenRoot :: Ast.Root -> CodeGenContext Cfg
codeGenRoot = codeGenStmts . Ast.stmts

codeGenStmts :: [ Ast.Stmt ] -> CodeGenContext Cfg 
codeGenStmts stmts = do { cfgs <- codeGenStmts' stmts; return $ foldl' Cfg.concat (Cfg.empty defaultLoc) cfgs }

codeGenStmts' :: [ Ast.Stmt ] -> CodeGenContext [ Cfg ]
codeGenStmts' = mapM codeGenStmt

-- | A simple dispatcher for code gen statements
-- see `codeGenStmt<TheStatementKindYouWantToInspect>`
codeGenStmt :: Ast.Stmt -> CodeGenContext Cfg
codeGenStmt (Ast.StmtIf     stmtIf    ) = return $ Cfg.empty defaultLoc
codeGenStmt (Ast.StmtCall   stmtCall  ) = return $ Cfg.empty defaultLoc
codeGenStmt (Ast.StmtDecvar stmtDecVar) = codeGenStmtDecvar stmtDecVar
codeGenStmt (Ast.StmtAssign stmtAssign) = return $ Cfg.empty defaultLoc
codeGenStmt _                           = return $ Cfg.empty defaultLoc

dummyTmpVar :: Bitcode.TmpVariable
dummyTmpVar = Bitcode.TmpVariable Fqn.nativeInt defaultLoc

dummyActualType :: ActualType
dummyActualType = ActualType.int

-- | A simple dispatcher for code gen expressions
-- see `codeGenExp<TheExpressionsKindYouWantToInspect>`
codeGenExp :: Ast.Exp -> CodeGenState -> (Cfg, Bitcode.TmpVariable, ActualType)
codeGenExp (Ast.ExpInt   expInt  ) _ = codeGenExpInt expInt
codeGenExp (Ast.ExpStr   expStr  ) _ = codeGenExpStr expStr
codeGenExp (Ast.ExpVar   expVar  ) s = (Cfg.empty defaultLoc, dummyTmpVar, dummyActualType)
codeGenExp (Ast.ExpCall  expCall ) s = (Cfg.empty defaultLoc, dummyTmpVar, dummyActualType)
codeGenExp _ _                       = (Cfg.empty defaultLoc, dummyTmpVar, dummyActualType)

-- | during stmt assign, it could be the case that
-- a simple variable is also defined. if so, we need
-- to insert it into the symbol table.
createBitcodeVarIfNeeded :: Ast.VarSimpleContent -> CodeGenContext ()
createBitcodeVarIfNeeded v = do { ctx <- get; put $ let 
    bitcodeVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable Fqn.nativeInt (Ast.varName v))
    bitcodeVarExists = SymbolTable.varExists (Ast.varName v) (symbolTable ctx)
    symbolTable' = SymbolTable.insertVar (Ast.varName v) bitcodeVar ActualType.int (symbolTable ctx)
    in case bitcodeVarExists of { True -> ctx; False -> ctx { symbolTable = symbolTable' } }
}

-- | dynamic languages often have variable declarations
-- "hide" in plain assignment syntax - this means that
-- assignment scanned according to their "ast order"
-- will insert the assigned variable to the symbol table
-- (if it hasn't been inserted before)
codeGenStmtAssignToSimpleVar :: Ast.VarSimpleContent -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToSimpleVar astSimpleVar exp = do {
    createBitcodeVarIfNeeded astSimpleVar;
    ctx <- get; return $ let
        (cfg, t, _) = codeGenExp exp ctx;
        varName = Ast.varName astSimpleVar;
        location = Token.getVarNameLocation varName;
        dst = SymbolTable.lookupBitcodeVar (Token.getVarNameToken varName) (symbolTable ctx);
        output = case dst of { Nothing -> Bitcode.TmpVariableCtor t; Just dst' -> dst' }
        content = Bitcode.AssignContent output t;
        instruction = Bitcode.Instruction location $ Bitcode.Assign content;
        in Cfg.concat cfg (Cfg.atom $ Node instruction)
}

codeGenStmtAssignToFieldVar :: Ast.VarFieldContent -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToFieldVar v e = undefined

-- codeGenStmtAssignToSubscriptVar :: Ast.VarSubscriptContent -> Ast.Exp -> CodeGenState -> (Cfg, Bitcode.TmpVariable, ActualType)
-- codeGenStmtAssignToSubscriptVar v e ctx = undefined

codeGenStmtAssign' :: Ast.Var -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssign' (Ast.VarSimple    v) e = codeGenStmtAssignToSimpleVar    v e
codeGenStmtAssign' (Ast.VarField     v) e = codeGenStmtAssignToFieldVar     v e
codeGenStmtAssign' _ _ = undefined -- codeGenStmtAssignToSubscriptVar v e

-- | dynamic languages often have variable declarations
-- "hide" in plain assignment syntax - this means that
-- assignment scanned according to their "ast order"
-- will insert the assigned variable to the symbol table
-- (if it hasn't been inserted before)
codeGenStmtAssign :: Ast.StmtAssignContent -> CodeGenContext Cfg
codeGenStmtAssign s = codeGenStmtAssign' (Ast.stmtAssignLhs s) (Ast.stmtAssignRhs s) 

-- |
-- * Monadic operation
--
-- * adds declared variable to symbol table
--
-- * Generating the initial value (if it exists) is non monadic
--
codeGenStmtDecvar :: Ast.DecVarContent -> CodeGenContext Cfg
codeGenStmtDecvar d = case Ast.decVarInitValue d of
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
    varName = Ast.decVarName decVar
    tokenName = Token.getVarNameToken varName
    nominalType = Token.getNominalTyToken (Ast.decVarNominalType decVar)
    bitcodeVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable Fqn.nativeInt varName
    actualType = SymbolTable.lookupActualType nominalType (symbolTable ctx)
    symbolTable' = SymbolTable.insert tokenName bitcodeVar actualType (symbolTable ctx)
    in ctx { symbolTable = symbolTable' }

-- | Non monadic helper function for updating the symbol table
codeGenDecVarInit' :: Token.VarName -> Ast.Exp -> CodeGenState -> CodeGenState
codeGenDecVarInit' varName initValue ctx = let
    (cfg, tmpVariable, actualType) = codeGenExp initValue ctx
    bitcodeVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable Fqn.nativeInt varName
    symbolTable' = SymbolTable.insertVar varName bitcodeVar actualType (symbolTable ctx)
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

codeGenExpInt :: Ast.ExpIntContent -> (Cfg, Bitcode.TmpVariable, ActualType)
codeGenExpInt expInt = let
    constInt = Ast.expIntValue expInt
    location = Token.constIntLocation constInt
    tmpVariable = Bitcode.TmpVariable Fqn.nativeInt location
    loadImmInt = Bitcode.LoadImmContentInt tmpVariable constInt
    instruction = Bitcode.Instruction location $ Bitcode.LoadImm loadImmInt
    in (Cfg.atom $ Node instruction, tmpVariable, ActualType.int)

codeGenExpStr :: Ast.ExpStrContent -> (Cfg, Bitcode.TmpVariable, ActualType)
codeGenExpStr expStr = let
    constStr = Ast.expStrValue expStr
    location = Token.constStrLocation constStr
    tmpVariable = Bitcode.TmpVariable Fqn.nativeStr location
    loadImmStr = Bitcode.LoadImmContentStr tmpVariable (Token.constStrValue constStr)
    instruction = Bitcode.Instruction location $ Bitcode.LoadImm loadImmStr
    in (Cfg.atom $ Node instruction, tmpVariable, ActualType.str)

-- | Temporarily
defaultLoc :: Location
defaultLoc = Location "" 0 0 0 0

