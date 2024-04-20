{-# OPTIONS -Werror=missing-fields #-}

module CodeGen

where

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
import SymbolTable ( SymbolTable, emptySymbolTable)

-- project (qualified) imports
import qualified Ast
import qualified Token
import qualified Bitcode
import qualified ActualType
import qualified SymbolTable

-- general imports
import Data.Maybe ( catMaybes )

-- general imports
import Data.List
import Control.Arrow
import Control.Monad.State.Lazy

data CodeGenState
   = CodeGenState
     {
         symbolTable :: SymbolTable,
         returnValue :: Maybe Bitcode.TmpVariable,
         returnTo :: Maybe Cfg,
         continueTo :: Maybe Cfg,
         breakTo :: Maybe Cfg,
         callables :: [ Callable ]
     }
     deriving ( Show )

-- | Internal use only
data GeneratedExp
   = GeneratedExp
     {
         generatedCfg :: Cfg,
         generatedValue :: Bitcode.Variable,
         inferredActualType :: ActualType
     }
     deriving ( Show )

-- | for the sake of documentation
type Callee = GeneratedExp
type Args = [ GeneratedExp ]

type CodeGenContext = State CodeGenState

initCodeGenState = CodeGenState {
    symbolTable = emptySymbolTable,
    returnValue = Nothing,
    returnTo = Nothing,
    continueTo = Nothing,
    breakTo = Nothing,
    callables = []
}

-- | API: generates code packed as a collection of callables
codeGen :: Asts -> Callables
codeGen = Callables . codeGen' . astsContent

-- | /all/ the files / asts from a given language are handled together
codeGen' :: [ Ast.Root ] -> [ Callable ]
codeGen' asts = callables $ execState (codeGenRoots asts) initCodeGenState

-- | no return value - computations are accummulated in the state
codeGenRoots :: [ Ast.Root ] -> CodeGenContext ()
codeGenRoots = mapM_ codeGenRoot 

-- |
-- * possibly /changing the original order/ of the file
--
-- * statements are collected and handled first
--
-- * declarations are handled second
--
codeGenRoot :: Ast.Root -> CodeGenContext ()
codeGenRoot ast = do { codeGenStmtsPart (Ast.stmts ast); codeGenDecsPart (Ast.decs ast) }

-- |
-- traversing the statements of the file has 2 effects:
--
-- * generating the "script" callable
--
-- * generating lambda and function callables
--
-- combine the 2 and update the state accordingly
--
codeGenStmtsPart :: [ Ast.Stmt ] -> CodeGenContext ()
codeGenStmtsPart stmts = do
    scriptCfg <- codeGenStmts stmts -- script part
    ctx <- get; -- function + lambda callables
    let callables' = (scriptToCallable scriptCfg) : (callables ctx) -- combine
    put $ ctx { callables = callables' } -- write back to state

-- | TODO: implement me ...
codeGenDecsPart :: [ Ast.Dec ] -> CodeGenContext ()
codeGenDecsPart _ = return () -- ignore for now 


-- | this function is called in two scenarios:
--
-- * codegen of the script part of the file
--
-- * codegen for the body of a callable
--
-- in either case, it returns the cfg, and accummulates callables
-- (and other info) to the state
codeGenStmts :: [ Ast.Stmt ] -> CodeGenContext Cfg 
codeGenStmts stmts = do { cfgs <- codeGenStmts' stmts; return $ foldl' Cfg.concat (Cfg.empty defaultLoc) cfgs }

codeGenStmts' :: [ Ast.Stmt ] -> CodeGenContext [ Cfg ]
codeGenStmts' = mapM codeGenStmt

-- | A simple dispatcher for code gen statements
-- see `codeGenStmt<TheStatementKindYouWantToInspect>`
codeGenStmt :: Ast.Stmt -> CodeGenContext Cfg
codeGenStmt (Ast.StmtIf     stmtIf    ) = return $ Cfg.empty defaultLoc
codeGenStmt (Ast.StmtCall   stmtCall  ) = codeGenStmtCall stmtCall
codeGenStmt (Ast.StmtDecvar stmtDecVar) = codeGenStmtDecvar stmtDecVar
codeGenStmt (Ast.StmtAssign stmtAssign) = return $ Cfg.empty defaultLoc
codeGenStmt _                           = return $ Cfg.empty defaultLoc

dummyTmpVar :: Bitcode.Variable
dummyTmpVar = Bitcode.TmpVariableCtor $ Bitcode.TmpVariable Fqn.nativeInt defaultLoc

dummyActualType :: ActualType
dummyActualType = ActualType.Any

-- | in fact, stmt call only wraps an expression call,
-- whose value doesn't get assigned anywhere ... it's really
-- all about wrapping the call to codeGenExpCall ...
codeGenStmtCall :: Ast.ExpCallContent -> CodeGenContext Cfg
codeGenStmtCall call = do { call' <- codeGenExpCall call; return $ generatedCfg call' }

-- | dispatch codegen exp handlers
codeGenExp :: Ast.Exp -> CodeGenContext GeneratedExp
codeGenExp (Ast.ExpInt    expInt    ) = return $ codeGenExpInt expInt -- ctx not needed
codeGenExp (Ast.ExpStr    expStr    ) = return $ codeGenExpStr expStr -- ctx not needed
codeGenExp (Ast.ExpVar    expVar    ) = codeGenExpVar expVar
codeGenExp (Ast.ExpCall   expCall   ) = codeGenExpCall expCall
codeGenExp (Ast.ExpLambda expLambda ) = codeGenExpLambda expLambda
codeGenExp _                          = return $ GeneratedExp (Cfg.empty defaultLoc) dummyTmpVar dummyActualType

-- |
-- two things happen during codegen of lambdas:
--
-- * the lambda callable is inserted to the state
--
-- * a temporary variable is created, indicative of the lambda (by location)
--
codeGenExpLambda :: Ast.ExpLambdaContent -> CodeGenContext GeneratedExp
codeGenExpLambda e = do { insertLambdaCallable e; codeGenExpLambda' e }

-- | insert the callable to the state
insertLambdaCallable :: Ast.ExpLambdaContent -> CodeGenContext ()
insertLambdaCallable expLambda = do
    lambdaCfg <- codeGenStmts (Ast.expLambdaBody expLambda)
    ctx <- get;
    let callables' = (lambdaToCallable lambdaCfg) : (callables ctx) -- combine
    put $ ctx { callables = callables' } -- write back to state

-- |
-- * generate an indicative variable (via location).
--
-- * cfg is just a single nop instruction ...
--
codeGenExpLambda' :: Ast.ExpLambdaContent -> CodeGenContext GeneratedExp
codeGenExpLambda' expLambda = do
    let location = (Ast.expLambdaLocation expLambda)
    let tmpVariable = Bitcode.TmpVariable (Fqn "lambda") location
    let variable = Bitcode.TmpVariableCtor tmpVariable
    let actualType = ActualType.Lambda $ ActualType.LambdaContent location
    return $ GeneratedExp (Cfg.empty location) variable actualType

-- | whenever something goes wrong, or simply not supported
-- we can generate a non deterministic expression
nondet :: Location -> GeneratedExp
nondet location = let
    nondetFunc = Token.VarName (Token.Named "nondet" location)
    arbitraryValue = Token.VarName (Token.Named "arbitrary_value" location)
    callee = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable Fqn.any nondetFunc
    output = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable Fqn.any arbitraryValue
    call = Bitcode.Call (Bitcode.CallContent output callee [])
    instruction = Bitcode.Instruction location call
    cfg = Cfg.atom (Cfg.Node instruction)
    in GeneratedExp cfg output ActualType.Any 

-- | for some reason, the variable needed to generate an exp
-- does not exist in the symbol table. this is (probably? surely?) an error.
-- best thing we can do is use some universal variable to capture all
-- the missing variables
codeGenExpVarSimpleMissing :: Location -> GeneratedExp
codeGenExpVarSimpleMissing = nondet

codeGenExpVarSimpleExisting :: Token.VarName -> Bitcode.Variable -> ActualType -> GeneratedExp
codeGenExpVarSimpleExisting v b t = GeneratedExp (Cfg.empty (Token.getVarNameLocation v)) b t 

codeGenExpVarSimple :: Ast.VarSimpleContent -> CodeGenState -> GeneratedExp
codeGenExpVarSimple v ctx = case SymbolTable.lookupVar (Ast.varName v) (symbolTable ctx) of
    Nothing -> codeGenExpVarSimpleMissing (Token.getVarNameLocation (Ast.varName v))
    Just (bitcodeVar, actualType) -> codeGenExpVarSimpleExisting (Ast.varName v) bitcodeVar actualType

codeGenExpVarField :: Ast.VarFieldContent -> CodeGenContext GeneratedExp
codeGenExpVarField v = do
    v' <- codeGenExp (Ast.ExpVar (Ast.varFieldLhs v))
    let fieldName = Ast.varFieldName v
    let inputFqn = Bitcode.variableFqn (generatedValue v')
    let fieldNameContent = Token.content (Token.getFieldNameToken fieldName)
    let outputFqn = Fqn ((Fqn.content inputFqn) ++ "." ++ fieldNameContent)
    let locationInput = Ast.locationVar (Ast.actualExpVar (Ast.varFieldLhs v))
    let input = Bitcode.TmpVariableCtor (Bitcode.TmpVariable inputFqn locationInput)
    let locationOutput = Ast.varFieldLocation v
    let output = Bitcode.TmpVariableCtor (Bitcode.TmpVariable outputFqn locationOutput)
    let fieldReadContent = Bitcode.FieldReadContent output input fieldName
    let fieldRead = Bitcode.FieldRead fieldReadContent
    let cfg = Cfg.atom (Cfg.Node (Bitcode.Instruction locationOutput fieldRead))
    return $ GeneratedExp cfg output ActualType.Any 

-- | dispatch codegen (exp) var handlers
codeGenExpVar :: Ast.ExpVarContent -> CodeGenContext GeneratedExp
codeGenExpVar (Ast.ExpVarContent (Ast.VarSimple    v)) = do { ctx <- get; return $ codeGenExpVarSimple v ctx }
codeGenExpVar (Ast.ExpVarContent (Ast.VarField     v)) = codeGenExpVarField v
codeGenExpVar (Ast.ExpVarContent (Ast.VarSubscript v)) = undefined -- codeGenExpVarSubscript v ctx

-- | code gen exps ( plural )
codeGenExps :: [ Ast.Exp ] -> CodeGenContext [ GeneratedExp ] 
codeGenExps = mapM codeGenExp

thirdPartyContent :: String -> ActualType
thirdPartyContent = ActualType.ThirdPartyImport . ActualType.ThirdPartyImportContent

-- | handle special javascript call: `require`
getReturnActualType' :: Args -> ActualType
getReturnActualType' [(GeneratedExp _ _ (NativeTypeConstStr value))] = thirdPartyContent ("npm." ++ value)
getReturnActualType' _ = ActualType.Any

-- | normal case currently ignores overloading
getReturnActualType'' :: Callee -> ActualType
getReturnActualType'' (GeneratedExp _ _ (ActualType.Function f)) = (ActualType.returnType f)
getReturnActualType'' _ = ActualType.Any

-- | separate javascript `require` calls
getReturnActualType :: Callee -> Args -> ActualType
getReturnActualType (GeneratedExp _ _ ActualType.Require) args = getReturnActualType' args
getReturnActualType callee _ = getReturnActualType'' callee

buildTheActualCall' :: Callee -> Args -> Bitcode.Variable -> Bitcode.CallContent
buildTheActualCall' callee args output = let
    callee' = generatedValue callee
    args' = Data.List.map generatedValue args
    in Bitcode.CallContent output callee' args'

buildTheActualCall :: Callee -> Args -> Bitcode.Variable -> Cfg
buildTheActualCall c a t = Cfg.atom $ Cfg.Node $ (Bitcode.Instruction defaultLoc) $ Bitcode.Call $ buildTheActualCall' c a t

codeGenExpCall' :: Callee -> Args -> Location -> GeneratedExp
codeGenExpCall' callee args location = let
    returnType = getReturnActualType callee args
    output = Bitcode.TmpVariableCtor $ Bitcode.TmpVariable (Fqn.fromActualType returnType) location
    actualCall = buildTheActualCall callee args output
    prepareCall = foldl' Cfg.concat (generatedCfg callee) (Data.List.map generatedCfg args)
    in GeneratedExp (prepareCall `Cfg.concat` actualCall) output returnType

codeGenExpCall :: Ast.ExpCallContent -> CodeGenContext GeneratedExp
codeGenExpCall call = do
    callee <- codeGenExp (Ast.callee call)
    args <- codeGenExps (Ast.args call)
    return $ codeGenExpCall' callee args (Ast.expCallLocation call)

-- | during stmt assign, it could be the case that
-- a simple variable is also defined. if so, we need
-- to insert it into the symbol table.
createBitcodeVarIfNeeded :: Ast.VarSimpleContent -> CodeGenContext ()
createBitcodeVarIfNeeded v = do { ctx <- get; put $ let 
    bitcodeVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable Fqn.nativeInt (Ast.varName v))
    bitcodeVarExists = SymbolTable.varExists (Ast.varName v) (symbolTable ctx)
    symbolTable' = SymbolTable.insertVar (Ast.varName v) bitcodeVar ActualType.Any (symbolTable ctx)
    in case bitcodeVarExists of { True -> ctx; False -> ctx { symbolTable = symbolTable' } }
}

-- | dynamic languages often have variable declarations
-- "hide" in plain assignment syntax - this means that
-- assignment scanned according to their "ast order"
-- will insert the assigned variable to the symbol table
-- (if it hasn't been inserted before)
codeGenStmtAssignToSimpleVar :: Ast.VarSimpleContent -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToSimpleVar astSimpleVar init = do
    createBitcodeVarIfNeeded astSimpleVar;
    init' <- codeGenExp init;
    ctx <- get;
    let varName = Ast.varName astSimpleVar;
    let location = Token.getVarNameLocation varName;
    let dst = SymbolTable.lookupVar varName (symbolTable ctx);
    let output = case dst of { Nothing -> generatedValue init'; Just dst' -> fst dst' }
    let content = Bitcode.AssignContent output (generatedValue init');
    let instruction = Bitcode.Instruction location $ Bitcode.Assign content;
    return $ Cfg.concat (generatedCfg init') (Cfg.atom $ Node instruction)

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
codeGenDecVarNoInit d loc = do { codeGenDecVarNoInit' d; return $ Cfg.empty loc }

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
    init' <- codeGenExp init;
    let fqn = Fqn.fromActualType (inferredActualType init');
    codeGenDecVarInit' varName init;
    return $ codeGenDecVarInit'' varName fqn (generatedCfg init', generatedValue init')

-- | pure non-monadic function, just update
-- the symbol table with the declared variable
codeGenDecVarNoInit' :: Ast.DecVarContent -> CodeGenContext ()
codeGenDecVarNoInit' decVar = do
    ctx <- get;
    let varName = Ast.decVarName decVar
    let nominalType = Ast.decVarNominalType decVar
    let actualType = SymbolTable.lookupNominalType nominalType (symbolTable ctx)
    let actualType' = case actualType of { Nothing -> ActualType.Any; Just t -> t }
    let bitcodeVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn.fromActualType actualType') varName
    let symbolTable' = SymbolTable.insertVar varName bitcodeVar actualType' (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }

-- | Non monadic helper function for updating the symbol table
codeGenDecVarInit' :: Token.VarName -> Ast.Exp -> CodeGenContext ()
codeGenDecVarInit' varName initValue = do
    init' <- codeGenExp initValue;
    ctx <- get;
    let actualType = inferredActualType init';
    let fqn = Fqn.fromActualType actualType;
    let bitcodeVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let symbolTable' = SymbolTable.insertVar varName bitcodeVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }

-- | Non monadic helper function for creating the cfg of the init value
codeGenDecVarInit'' :: Token.VarName -> Fqn -> (Cfg, Bitcode.Variable) -> Cfg
codeGenDecVarInit'' varName fqn (cfg, tmpVariable) = let
    srcVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    assignContent = Bitcode.AssignContent { assignOutput = srcVariable, assignInput = tmpVariable }
    assign = Bitcode.Assign assignContent
    location' = Token.getVarNameLocation varName
    instruction = Bitcode.Instruction { Bitcode.location = location', instructionContent = assign }
    in cfg `Cfg.concat` (Cfg.atom (Node instruction))

codeGenExpInt :: Ast.ExpIntContent -> GeneratedExp
codeGenExpInt expInt = let
    constInt = Ast.expIntValue expInt
    constIntValue = Token.constIntValue constInt
    location = Token.constIntLocation constInt
    tmpVariable = Bitcode.TmpVariable Fqn.nativeInt location
    variable = Bitcode.TmpVariableCtor tmpVariable
    loadImmInt = Bitcode.IntContent tmpVariable constInt
    instruction = Bitcode.Instruction location $ Bitcode.LoadImmInt loadImmInt
    actualType = ActualType.NativeTypeConstInt constIntValue
    in GeneratedExp (Cfg.atom $ Node instruction) variable actualType

codeGenExpStr :: Ast.ExpStrContent -> GeneratedExp
codeGenExpStr expStr = let
    constStr = Ast.expStrValue expStr
    constStrValue = Token.constStrValue constStr
    location = Token.constStrLocation constStr
    tmpVariable = Bitcode.TmpVariable Fqn.nativeStr location
    variable = Bitcode.TmpVariableCtor tmpVariable
    loadImmStr = Bitcode.StrContent tmpVariable constStr
    instruction = Bitcode.Instruction location $ Bitcode.LoadImmStr loadImmStr
    actualType = ActualType.NativeTypeConstStr constStrValue
    in GeneratedExp (Cfg.atom $ Node instruction) variable actualType

-- minor non interesting helper functions here

scriptToCallable :: Cfg -> Callable
scriptToCallable cfg = let
    filename = Location.filename (Cfg.location cfg)
    content = Callable.ScriptContent filename cfg
    in Callable.Script content

lambdaToCallable :: Cfg -> Callable
lambdaToCallable cfg = let
    location = Cfg.location cfg
    content = Callable.LambdaContent cfg location
    in Callable.Lambda content

-- | Temporarily
defaultLoc :: Location
defaultLoc = Location "" 0 0 0 0

