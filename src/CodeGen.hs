{-# OPTIONS -Werror=missing-fields #-}
{-# OPTIONS -Wno-missing-signatures #-}

module CodeGen

where

-- project imports
import Fqn
import Cfg 
import Asts
import Callable
import Location
import ActualType hiding ( returnType )
import SymbolTable ( SymbolTable, emptySymbolTable)

-- project (qualified) imports
import qualified Ast
import qualified Token
import qualified Bitcode
import qualified ActualType
import qualified SymbolTable

-- general imports
import Data.Maybe
import Data.List hiding ( init )
import Prelude hiding ( exp, init )
import Control.Monad.State.Lazy

-- general (qualified) imports
import qualified Data.Set
import qualified Data.Map

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

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState {
    symbolTable = emptySymbolTable,
    returnValue = Nothing,
    returnTo = Nothing,
    continueTo = Nothing,
    breakTo = Nothing,
    callables = []
}

-- | API: generates code packed as a collection of callables
codeGen :: Ast.Root -> Callables
codeGen ast = Callables (callables (execState (codeGenRoot ast) initCodeGenState))

codeGenRoot :: Ast.Root -> CodeGenContext ()
codeGenRoot = codeGenStmtsPart . Ast.stmts

codeGenStmtsPart :: [ Ast.Stmt ] -> CodeGenContext ()
codeGenStmtsPart stmts = do
    scriptCfg <- codeGenStmts defaultLoc stmts -- script part
    ctx <- get; -- function + lambda callables
    let callables' = (scriptToCallable scriptCfg) : (callables ctx) -- combine
    put $ ctx { callables = callables' } -- write back to state

codeGenStmtClass :: Ast.StmtClassContent -> CodeGenContext Cfg
codeGenStmtClass stmtClass = do
    beginScope
    insertSelf (Ast.stmtClassName stmtClass)
    codeGenDataMembers (Ast.stmtClassDataMembers stmtClass)
    codeGenMethods (Ast.stmtClassMethods stmtClass)
    endScope
    return $ Cfg.empty (Token.getClassNameLocation (Ast.stmtClassName stmtClass))

insertSelf :: Token.ClassName -> CodeGenContext ()
insertSelf className' = do
    ctx <- get
    let classNameToken = Token.getClassNameToken className'
    let selfVarName = Token.VarName (Token.Named "self" (Token.location classNameToken))
    let thisVarName = Token.VarName (Token.Named "this" (Token.location classNameToken))
    let classFqn = Fqn (Token.content classNameToken)
    let selfSrcVar = Bitcode.SrcVariable classFqn selfVarName
    let thisSrcVar = Bitcode.SrcVariable classFqn thisVarName
    let selfVar = Bitcode.SrcVariableCtor selfSrcVar
    let thisVar = Bitcode.SrcVariableCtor thisSrcVar
    let emptyDataMembers = ActualType.DataMembers Data.Set.empty
    let emptyMethods = ActualType.Methods Data.Map.empty
    let emptySupers = ActualType.Supers Data.Set.empty
    let actualType = ActualType.Class (ActualType.ClassContent className' emptySupers emptyMethods emptyDataMembers)
    let symbolTable' = SymbolTable.insertVar selfVarName selfVar actualType (symbolTable ctx)
    let symbolTable2 = SymbolTable.insertVar thisVarName thisVar actualType symbolTable'
    put $ ctx { symbolTable = symbolTable2 }
    return ()

codeGenMethods :: Ast.Methods -> CodeGenContext ()
codeGenMethods = codeGenMethods' . Data.Map.elems . Ast.actualMethods

codeGenDataMembers :: Ast.DataMembers -> CodeGenContext ()
codeGenDataMembers _ = return () -- TODO: implement me ...

codeGenMethods' :: [ Ast.StmtMethodContent ] -> CodeGenContext ()
codeGenMethods' = mapM_ codeGenStmtMethod

insertEmptyClass :: Token.ClassName -> CodeGenContext ()
insertEmptyClass className = do
    ctx <- get
    let classNameToken = Token.getClassNameToken className
    let classVarName = Token.VarName classNameToken
    let classFqn = Fqn (Token.content classNameToken)
    let classSrcVar = Bitcode.SrcVariable classFqn classVarName
    let classVar = Bitcode.SrcVariableCtor classSrcVar
    let emptyDataMembers = ActualType.DataMembers Data.Set.empty
    let emptyMethods = ActualType.Methods Data.Map.empty
    let emptySupers = ActualType.Supers Data.Set.empty
    let actualType = ActualType.Class (ActualType.ClassContent className emptySupers emptyMethods emptyDataMembers)
    let symbolTable' = SymbolTable.insertClass className classVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }

codeGenStmtMethodSingle :: Ast.StmtMethodContent -> CodeGenContext Cfg
codeGenStmtMethodSingle stmtMethod = do
    insertEmptyClass (Ast.hostingClassName stmtMethod)
    beginScope
    params' <- codeGenParams (Ast.stmtMethodLocation stmtMethod) (Ast.stmtMethodParams stmtMethod)
    body <- codeGenStmts (Ast.stmtMethodLocation stmtMethod) (Ast.stmtMethodBody stmtMethod)
    endScope
    ctx <- get
    let symbolTable' = symbolTable ctx
    let callables' = (stmtMethodToCallable stmtMethod symbolTable' (Cfg.concat params' body)) : (callables ctx)
    put (ctx { callables = callables' })
    return $ Cfg.empty (Ast.stmtMethodLocation stmtMethod)

codeGenStmtMethodV2 :: Ast.StmtMethodContent -> CodeGenContext ()
codeGenStmtMethodV2 stmtMethod = do
    -- instrument a single return variable
    -- all values returned from the method will assign to it
    let location = Ast.stmtMethodLocation stmtMethod
    let returnValueFqn = Fqn "returnValue"
    let returnValueTmpVar = Bitcode.TmpVariable returnValueFqn location
    let returnedValue = Bitcode.TmpVariableCtor returnValueTmpVar
    -- instrument a single return instruction
    -- note that even void method will instrument this part
    let returnedContent = Bitcode.ReturnContent (Just returnedValue)
    let returnInstruction = Bitcode.Instruction location (Bitcode.Return returnedContent)
    let returnSite = Cfg.atom (Cfg.Node returnInstruction)
    -- recursive code gen params + body
    beginScope
    params' <- codeGenParams (Ast.stmtMethodLocation stmtMethod) (Ast.stmtMethodParams stmtMethod)
    original <- get
    put $ original { returnTo = Just returnSite, returnValue = Just returnValueTmpVar }
    body <- codeGenStmts (Ast.stmtMethodLocation stmtMethod) (Ast.stmtMethodBody stmtMethod)
    ctx' <- get
    put $ ctx' { returnTo = returnTo original, returnValue = returnValue original }
    endScope
    ctx <- get
    let hostingClass = (Ast.hostingClassName stmtMethod)
    let rawClassName = Token.getClassNameToken hostingClass
    let emptyDataMembers = ActualType.DataMembers Data.Set.empty
    let emptyMethods = ActualType.Methods Data.Map.empty
    let emptySupers = ActualType.Supers Data.Set.empty
    let classType = ActualType.Class (ActualType.ClassContent hostingClass emptySupers emptyMethods emptyDataMembers)
    let classVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn (Token.content rawClassName)) (Token.VarName rawClassName)
    let symbolTable' = SymbolTable.insertClass hostingClass classVar classType (symbolTable ctx)
    let symbolTable'' = case SymbolTable.lookupVar (Token.VarName rawClassName) (symbolTable ctx) of {
        Nothing -> symbolTable';
        _ -> (symbolTable ctx)
    }
    let callables' = (stmtMethodToCallable stmtMethod symbolTable' (Cfg.concat params' body)) : (callables ctx)
    put (ctx { callables = callables', symbolTable = symbolTable'' })

codeGenStmtMethod :: Ast.StmtMethodContent -> CodeGenContext ()
codeGenStmtMethod stmtMethod = do
    beginScope
    params' <- codeGenParams (Ast.stmtMethodLocation stmtMethod) (Ast.stmtMethodParams stmtMethod)
    body <- codeGenStmts (Ast.stmtMethodLocation stmtMethod) (Ast.stmtMethodBody stmtMethod)
    endScope
    ctx <- get
    let hostingClass = (Ast.hostingClassName stmtMethod)
    let rawClassName = Token.getClassNameToken hostingClass
    let emptyDataMembers = ActualType.DataMembers Data.Set.empty
    let emptyMethods = ActualType.Methods Data.Map.empty
    let emptySupers = ActualType.Supers Data.Set.empty
    let classType = ActualType.Class (ActualType.ClassContent hostingClass emptySupers emptyMethods emptyDataMembers)
    let classVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn (Token.content rawClassName)) (Token.VarName rawClassName) 
    let symbolTable' = SymbolTable.insertClass hostingClass classVar classType (symbolTable ctx)
    let symbolTable'' = case SymbolTable.lookupVar (Token.VarName rawClassName) (symbolTable ctx) of {
        Nothing -> symbolTable';
        _ -> (symbolTable ctx)
    }
    let location = Ast.stmtMethodLocation stmtMethod
    let nop = Bitcode.Instruction location Bitcode.Nop
    let entrypoint = Cfg.atom (Cfg.Node nop)
    let cfg = Cfg.concat entrypoint (Cfg.concat params' body)
    let callables' = (stmtMethodToCallable stmtMethod symbolTable' cfg) : (callables ctx)
    put (ctx { callables = callables', symbolTable = symbolTable'' })

codeGenStmts :: Location -> [ Ast.Stmt ] -> CodeGenContext Cfg 
codeGenStmts loc stmts = do { cfgs <- codeGenStmts' stmts; return $ foldl' Cfg.concat (Cfg.empty loc) cfgs }

codeGenStmts' :: [ Ast.Stmt ] -> CodeGenContext [ Cfg ]
codeGenStmts' = mapM codeGenStmt

-- | A simple dispatcher for code gen statements
-- see `codeGenStmt<TheStatementKindYouWantToInspect>`
codeGenStmt :: Ast.Stmt -> CodeGenContext Cfg
codeGenStmt (Ast.StmtIf     stmtIf    ) = codeGenStmtIf stmtIf
codeGenStmt (Ast.StmtExp    stmtExp   ) = codeGenStmtExp stmtExp
codeGenStmt (Ast.StmtFunc   stmtFunc  ) = codeGenStmtFunc stmtFunc
codeGenStmt (Ast.StmtClass  stmtClass ) = codeGenStmtClass stmtClass
codeGenStmt (Ast.StmtWhile  stmtWhile ) = codeGenStmtWhile stmtWhile
codeGenStmt (Ast.StmtBlock  stmtBlock ) = codeGenStmtBlock stmtBlock
codeGenStmt (Ast.StmtVardec stmtVardec) = codeGenStmtDecvar stmtVardec
codeGenStmt (Ast.StmtAssign stmtAssign) = codeGenStmtAssign stmtAssign
codeGenStmt (Ast.StmtImport stmtImport) = codeGenStmtImport stmtImport
codeGenStmt (Ast.StmtReturn stmtReturn) = codeGenStmtReturn stmtReturn
codeGenStmt (Ast.StmtMethod stmtMethod) = codeGenStmtMethodSingle stmtMethod
codeGenStmt _                           = return $ Cfg.empty defaultLoc

codeGenStmtExp :: Ast.Exp -> CodeGenContext Cfg
codeGenStmtExp e = do { e' <- codeGenExp e; return $ generatedCfg e' }

codeGenStmtBlock :: Ast.StmtBlockContent -> CodeGenContext Cfg
codeGenStmtBlock block = codeGenStmts (Ast.stmtBlockLocation block) (Ast.stmtBlockContent block)

codeGenStmtReturnNothing :: Ast.StmtReturnContent -> CodeGenContext Cfg
codeGenStmtReturnNothing stmtReturn = do
    let location' = Ast.stmtReturnLocation stmtReturn
    let content' = Bitcode.ReturnContent Nothing
    let returnInstruction = Bitcode.Return content'
    let instruction = Bitcode.Instruction location' returnInstruction
    return $ Cfg.atom (Cfg.Node instruction)

codeGenStmtReturnValue :: Location -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtReturnValue loc exp = do
    returnValue' <- codeGenExp exp
    let content' = Bitcode.ReturnContent (Just (generatedValue returnValue'))
    let returnInstruction = Bitcode.Return content'
    let instruction = Bitcode.Instruction loc returnInstruction
    let returnCfg = Cfg.atom (Cfg.Node instruction)
    return $ (generatedCfg returnValue') `Cfg.concat` returnCfg

codeGenStmtReturn :: Ast.StmtReturnContent -> CodeGenContext Cfg
codeGenStmtReturn stmtReturn = case (Ast.stmtReturnValue stmtReturn) of
    Nothing -> codeGenStmtReturnNothing stmtReturn
    Just returnedValue -> codeGenStmtReturnValue (Ast.stmtReturnLocation stmtReturn) returnedValue

codeGenStmtIf :: Ast.StmtIfContent -> CodeGenContext Cfg
codeGenStmtIf stmtIf = do
    cond <- codeGenExp (Ast.stmtIfCond stmtIf)
    thenPart <- codeGenStmts (Ast.stmtIfLocation stmtIf) (Ast.stmtIfBody stmtIf)
    elsePart <- codeGenStmts (Ast.stmtIfLocation stmtIf) (Ast.stmtElseBody stmtIf)
    let assumeIfTaken = assumeIfTakenGen (generatedValue cond) (Ast.stmtIfLocation stmtIf)
    let assumeIfNotTaken = assumeIfNotTakenGen (generatedValue cond) (Ast.stmtIfLocation stmtIf)
    let thenPartCfg = assumeIfTaken `Cfg.concat` thenPart
    let elsePartCfg = assumeIfNotTaken `Cfg.concat` elsePart
    return ((generatedCfg cond) `Cfg.concat` (thenPartCfg `Cfg.parallel` elsePartCfg))

codeGenStmtWhile :: Ast.StmtWhileContent -> CodeGenContext Cfg
codeGenStmtWhile stmtWhile = do
    cond <- codeGenExp (Ast.stmtWhileCond stmtWhile)
    body <- codeGenStmts (Ast.stmtWhileLocation stmtWhile) (Ast.stmtWhileBody stmtWhile)
    return $ (generatedCfg cond) `Cfg.concat` body

assumeIfTakenGen :: Bitcode.Variable -> Location -> Cfg
assumeIfTakenGen cond loc = let
    content' = Bitcode.Assume $ Bitcode.AssumeContent cond True
    instruction = Bitcode.Instruction loc content'
    in Cfg.atom (Cfg.Node instruction)

assumeIfNotTakenGen :: Bitcode.Variable -> Location -> Cfg
assumeIfNotTakenGen cond loc = let
    content' = Bitcode.Assume $ Bitcode.AssumeContent cond False
    instruction = Bitcode.Instruction loc content'
    in Cfg.atom (Cfg.Node instruction)

codeGenAnnotations :: [ Ast.Exp ] -> CodeGenContext [ Callable.Annotation ]
codeGenAnnotations = mapM codeGenAnnotation

stringOrNothing :: Ast.Exp -> Maybe Token.ConstStr
stringOrNothing (Ast.ExpStr (Ast.ExpStrContent s)) = Just s
stringOrNothing _ = Nothing

keepStrings :: [ Ast.Exp ] -> [ Token.ConstStr ]
keepStrings exps = catMaybes (map stringOrNothing exps)

codeGenAnnotation :: Ast.Exp -> CodeGenContext Callable.Annotation
codeGenAnnotation exp = do
    exp' <- codeGenExp exp
    let fqn = ActualType.toFqn (inferredActualType exp')
    let args = case exp of { (Ast.ExpCall (Ast.ExpCallContent _ a _)) -> a; _ -> [] }
    return $ Callable.Annotation (Fqn.content fqn) (map Token.constStrValue (keepStrings args))

codeGenStmtFunc :: Ast.StmtFuncContent -> CodeGenContext Cfg
codeGenStmtFunc stmtFunc = do
    annotations <- codeGenAnnotations (Ast.stmtFuncAnnotations stmtFunc)
    beginScope
    params' <- codeGenParams (Ast.stmtFuncLocation stmtFunc) (Ast.stmtFuncParams stmtFunc)
    body <- codeGenStmts (Ast.stmtFuncLocation stmtFunc) (Ast.stmtFuncBody stmtFunc)
    endScope
    ctx <- get
    let callable = decFuncToCallable stmtFunc (Cfg.concat params' body) annotations
    let callables' = callable : (callables ctx)
    let funcName' = Token.VarName $ Token.getFuncNameToken (Ast.stmtFuncName stmtFunc)
    let funcFqn = Fqn (Token.content (Token.getVarNameToken funcName'))
    let funcVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable funcFqn funcName'
    let r = Token.getNominalTyToken (Ast.stmtFuncReturnType stmtFunc)
    let emptyDataMembers = ActualType.DataMembers Data.Set.empty
    let emptyMethods = ActualType.Methods Data.Map.empty
    let emptySupers = ActualType.Supers Data.Set.empty
    let returnType = ActualType.Class (ActualType.ClassContent (Token.ClassName r) emptySupers emptyMethods emptyDataMembers)
    let funcContent = ActualType.FunctionContent (Ast.stmtFuncName stmtFunc) (ActualType.Params []) returnType
    let funcType = ActualType.Function funcContent
    let symbolTable' = SymbolTable.insertVar funcName' funcVar funcType (symbolTable ctx)
    put (ctx { symbolTable = symbolTable', callables = callables' })
    return $ Cfg.empty (Token.getFuncNameLocation (Ast.stmtFuncName stmtFunc))

codeGenStmtImportAll :: String -> Location -> CodeGenContext Cfg
codeGenStmtImportAll src l = do
    ctx <- get
    let srcVarName = Token.VarName (Token.Named src l)
    let srcVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (Fqn src) srcVarName)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent src)
    let symbolTable' = SymbolTable.insertVar srcVarName srcVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return $ Cfg.empty l

codeGenStmtImportSpecific :: String -> String -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecific src specific l = do
    ctx <- get
    let specificVarName = Token.VarName (Token.Named specific l)
    let specificVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (Fqn specific) specificVarName)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent (src ++ "." ++ specific))
    let symbolTable' = SymbolTable.insertVar specificVarName specificVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return $ Cfg.empty l

codeGenStmtImportSpecificWithAlias :: String -> String -> String -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificWithAlias src specific alias l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (Fqn alias) aliasVarName)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent (src ++ "." ++ specific))
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return $ Cfg.empty l

codeGenStmtImportAllWithAlias :: String -> String -> Location -> CodeGenContext Cfg
codeGenStmtImportAllWithAlias src alias l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (Fqn alias) aliasVarName)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent src)
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return $ Cfg.empty l

codeGenStmtImport :: Ast.StmtImportContent -> CodeGenContext Cfg
codeGenStmtImport stmtImport = do
    let loc = Ast.stmtImportLocation stmtImport
    let src = Ast.stmtImportSource stmtImport
    let specific = Ast.stmtImportFromSource stmtImport
    let alias = Ast.stmtImportAlias stmtImport
    case specific of
        Nothing -> case alias of
            Nothing -> codeGenStmtImportAll src loc
            Just alias' -> codeGenStmtImportAllWithAlias src alias' loc
        Just specific' -> case alias of
            Nothing -> codeGenStmtImportSpecific src specific' loc
            Just alias' -> codeGenStmtImportSpecificWithAlias src specific' alias' loc

dummyTmpVar :: Bitcode.Variable
dummyTmpVar = Bitcode.TmpVariableCtor $ Bitcode.TmpVariable Fqn.nativeInt defaultLoc

dummyActualType :: ActualType
dummyActualType = someType

-- | dispatch codegen exp handlers
codeGenExp :: Ast.Exp -> CodeGenContext GeneratedExp
codeGenExp (Ast.ExpInt    expInt    ) = return $ codeGenExpInt expInt -- ctx not needed
codeGenExp (Ast.ExpStr    expStr    ) = return $ codeGenExpStr expStr -- ctx not needed
codeGenExp (Ast.ExpVar    expVar    ) = codeGenExpVar expVar
codeGenExp (Ast.ExpCall   expCall   ) = codeGenExpCall expCall
codeGenExp (Ast.ExpBinop  expBinop  ) = codeGenExpBinop expBinop
codeGenExp (Ast.ExpAssign expAssign ) = codeGenExpAssign expAssign
codeGenExp (Ast.ExpLambda expLambda ) = codeGenExpLambda expLambda
codeGenExp _                          = return $ GeneratedExp (Cfg.empty defaultLoc) dummyTmpVar dummyActualType

codeGenExpAssignToSimpleVar :: Token.VarName -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssignToSimpleVar varName init = do
    init' <- codeGenExp init
    ctx <- get;
    let initCfg = generatedCfg init'
    let initVariable = generatedValue init'
    let actualType = inferredActualType init'
    let fqn = ActualType.toFqn actualType
    let location' = Token.getVarNameLocation varName
    let existingVar = SymbolTable.lookupVar varName (symbolTable ctx)
    let newVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let symbolTable' = SymbolTable.insertVar varName newVariable actualType (symbolTable ctx)
    let updateSymbolTable = ctx { symbolTable = symbolTable' }
    let dontChangeCtx = ctx
    let actualVar = case existingVar of { Nothing -> newVariable; Just (v, _) -> v }
    put $ case existingVar of { Nothing -> updateSymbolTable; _ -> dontChangeCtx }
    let cfg = initCfg `Cfg.concat` (createAssignCfg location' actualVar initVariable)
    return $ GeneratedExp cfg actualVar actualType

codeGenExpAssignToFieldVar :: Ast.VarFieldContent -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssignToFieldVar var exp = do
    exp' <- codeGenExp exp
    var' <- codeGenExp (Ast.varFieldLhs var)
    let location' = Ast.varFieldLocation var
    let lhsVar = generatedValue var'
    let expVar = generatedValue exp'
    let fieldName = Ast.varFieldName var
    let content' = Bitcode.FieldWriteContent lhsVar fieldName expVar
    let fieldWrite = Bitcode.FieldWrite content'
    let instruction = Bitcode.Instruction location' fieldWrite
    let expCfg = generatedCfg exp'
    let varCfg = generatedCfg var'
    let cfg = Cfg.atom (Cfg.Node instruction)
    let finalCfg = expCfg `Cfg.concat` varCfg `Cfg.concat` cfg
    let actualType = inferredActualType var'
    return $ GeneratedExp finalCfg lhsVar actualType

codeGenExpAssignToSubscriptVar :: Ast.VarSubscriptContent -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssignToSubscriptVar subscriptVar value = do
    value' <- codeGenExp value
    index <- codeGenExp (Ast.varSubscriptIdx subscriptVar)
    lhs <- codeGenExp (Ast.varSubscriptLhs subscriptVar)
    let valueCfg = generatedCfg value'
    let indexCfg = generatedCfg index
    let lhsVar = generatedValue lhs
    let lhsCfg = generatedCfg lhs
    let lhsType = inferredActualType lhs
    let fqn = ActualType.toFqn lhsType
    let location' = Ast.varSubscriptLocation subscriptVar
    let varName = Token.VarName $ Token.Named (Fqn.content fqn) location'
    let output = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let subscriptIdx = generatedValue index
    let input = generatedValue value'
    let actualType = inferredActualType value'
    let content' = Bitcode.SubscriptWriteContent output subscriptIdx input
    let subscriptWrite = Bitcode.SubscriptWrite content'
    let instruction = Bitcode.Instruction location' subscriptWrite
    let cfg = valueCfg `Cfg.concat` indexCfg `Cfg.concat` lhsCfg 
    let finalCfg = cfg `Cfg.concat` (Cfg.atom (Cfg.Node instruction))
    return $ GeneratedExp finalCfg lhsVar actualType

codeGenExpAssign' :: Ast.Var -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssign' (Ast.VarSimple    v) e = codeGenExpAssignToSimpleVar (Ast.varName v) e
codeGenExpAssign' (Ast.VarField     v) e = codeGenExpAssignToFieldVar v e
codeGenExpAssign' (Ast.VarSubscript v) e = codeGenExpAssignToSubscriptVar v e

codeGenExpAssign :: Ast.ExpAssignContent -> CodeGenContext GeneratedExp
codeGenExpAssign e = codeGenExpAssign' (Ast.expAssignLhs e) (Ast.expAssignRhs e)

codeGenExpBinop :: Ast.ExpBinopContent -> CodeGenContext GeneratedExp
codeGenExpBinop expBinop = do
    lhs <- codeGenExp (Ast.expBinopLeft expBinop)
    rhs <- codeGenExp (Ast.expBinopRight expBinop)
    let location' = (Ast.expBinopLocation expBinop)
    let binopLhs = generatedValue lhs
    let binopRhs = generatedValue rhs
    let binopOutput = Bitcode.TmpVariableCtor $ Bitcode.TmpVariable (Fqn "MMM") location'
    let binop = Bitcode.BinopContent binopOutput binopLhs binopRhs
    let content' = Bitcode.Binop binop
    let instruction = Bitcode.Instruction location' content'
    let cfg = Cfg.atom (Cfg.Node instruction)
    let actualType = someType
    let binopCfg = (generatedCfg lhs) `Cfg.concat` (generatedCfg rhs)
    return $ GeneratedExp (binopCfg `Cfg.concat` cfg) binopOutput actualType

-- |
-- two things happen during codegen of lambdas:
--
-- * the lambda callable is inserted to the state
--
-- * a temporary variable is created, indicative of the lambda (by location)
--
codeGenExpLambda :: Ast.ExpLambdaContent -> CodeGenContext GeneratedExp
codeGenExpLambda e = do { insertLambdaCallable e; codeGenExpLambda' e }

-- beginScope :: CodeGenContext ()
-- endScope   :: CodeGenContext ()

beginScope = do { ctx <- get; put $ ctx { symbolTable = SymbolTable.beginScope (symbolTable ctx) } }
endScope   = do { ctx <- get; put $ ctx { symbolTable = SymbolTable.endScope   (symbolTable ctx) } }

handleLambda :: Ast.ExpLambdaContent -> CodeGenContext Callable
handleLambda lambda = do
    paramDeclsCfg <- codeGenLambdaParams lambda
    lambdaBodyCfg <- codeGenStmts (Ast.expLambdaLocation lambda) (Ast.expLambdaBody lambda)
    return $ lambdaToCallable (Cfg.concat paramDeclsCfg lambdaBodyCfg) (Ast.expLambdaLocation lambda)

handleLambdaCallable :: Ast.ExpLambdaContent -> CodeGenContext Callable
handleLambdaCallable lambda = do { beginScope; callable <- handleLambda lambda; endScope; return callable }

-- | insert the callable to the state
insertLambdaCallable :: Ast.ExpLambdaContent -> CodeGenContext ()
insertLambdaCallable expLambda = do { c <- handleLambdaCallable expLambda; ctx <- get; put $ ctx { callables = c:(callables ctx) } }

codeGenLambdaParams :: Ast.ExpLambdaContent -> CodeGenContext Cfg
codeGenLambdaParams lambda = codeGenParams (Ast.expLambdaLocation lambda) (Ast.expLambdaParams lambda)

codeGenParams :: Location -> [ Ast.Param ] -> CodeGenContext Cfg
codeGenParams loc params' = do
    cfgs <- codeGenParams' 0 params'
    return $ foldl' Cfg.concat (Cfg.empty loc) cfgs

codeGenParams' :: Word -> [ Ast.Param ] -> CodeGenContext [ Cfg ]
codeGenParams' _ [] = return []
codeGenParams' i (p:ps) = do { cfg <- codeGenParam i p; cfgs <- codeGenParams' (i+1) ps; return (cfg:cfgs) }

-- | The nominal type is represented as an Ast.Var
codeGenParam' :: Word -> Token.ParamName -> Ast.Var -> CodeGenContext Cfg
codeGenParam' i name nominalType = do
    ctx <- get
    nominalType' <- codeGenVar nominalType
    let actualType = inferredActualType nominalType'
    let fqn = ActualType.toFqn actualType
    let location' = Token.getParamNameLocation name
    let paramVar = Bitcode.ParamVariable fqn i name
    let v = Bitcode.ParamVariableCtor paramVar
    let paramDecl = Bitcode.ParamDecl $ Bitcode.ParamDeclContent paramVar
    let instruction = Bitcode.Instruction location' paramDecl
    put (ctx { symbolTable = SymbolTable.insertParam name v actualType (symbolTable ctx) })
    return (Cfg.atom (Cfg.Node instruction))

codeGenParam'' :: Word -> Ast.Param -> CodeGenContext Cfg
codeGenParam'' paramSerialIdx' param = do
    ctx <- get
    let paramName' = Ast.paramName param
    let paramRawName = Token.content $ Token.getParamNameToken paramName'
    let location' = Token.getParamNameLocation paramName'
    let nominalType = Ast.paramNominalType param
    let nominalTypeName = Token.content (Token.getNominalTyToken nominalType)
    -- some languages explictly pass self / this as the first parameter
    -- also, some languages force the use of self.<method> (python)
    -- while other languages (Java) enable both variants (this.<method> and just <method>)
    -- even a strictly type-hinted python code will *not* annotate self - this will
    -- cause a type override in the symbol table where "self" will have type "any"
    let fromSelf = SymbolTable.lookupVar (Token.VarName (Token.Named "self" location')) (symbolTable ctx)
    let fromNominal = SymbolTable.lookupNominalType nominalType (symbolTable ctx)
    let actualType = case paramRawName of {
        "self" -> case fromSelf of { Nothing -> fromNominal; Just (_,t) -> t };
        _ -> fromNominal
    }
    let fqn' = ActualType.toFqn actualType
    let fqn'' = Fqn (Token.content (Token.getParamNameToken paramName'))
    let fqn = case nominalTypeName == "any" of { True -> fqn''; False -> fqn' }
    let paramVar = Bitcode.ParamVariable fqn paramSerialIdx' paramName'
    let paramDecl = Bitcode.ParamDecl $ Bitcode.ParamDeclContent paramVar
    let instruction = Bitcode.Instruction location' paramDecl
    let v = Bitcode.ParamVariableCtor paramVar
    put (ctx { symbolTable = SymbolTable.insertParam paramName' v actualType (symbolTable ctx) })
    return (Cfg.atom (Cfg.Node instruction))

codeGenParam :: Word -> Ast.Param -> CodeGenContext Cfg
codeGenParam paramSerialIdx' param = case Ast.paramNominalTypeV2 param of
    Nothing -> codeGenParam'' paramSerialIdx' param
    Just nominalType -> codeGenParam' paramSerialIdx' (Ast.paramName param) nominalType

-- |
-- * generate an indicative variable (via location).
--
-- * cfg is just a single nop instruction ...
--
codeGenExpLambda' :: Ast.ExpLambdaContent -> CodeGenContext GeneratedExp
codeGenExpLambda' expLambda = do
    let location' = (Ast.expLambdaLocation expLambda)
    let tmpVariable = Bitcode.TmpVariable (Fqn "lambda") location'
    let variable = Bitcode.TmpVariableCtor tmpVariable
    let actualType = ActualType.Lambda $ ActualType.LambdaContent location'
    return $ GeneratedExp (Cfg.empty location') variable actualType

-- | whenever something goes wrong, or simply not supported
-- we can generate a non deterministic expression
nondet :: Token.VarName -> GeneratedExp
nondet varName = let
    location' = Token.getVarNameLocation varName
    rawName = Token.content (Token.getVarNameToken varName)
    nondetFunc = Token.VarName (Token.Named "nondet" location')
    arbitraryValue = Token.VarName (Token.Named "arbitrary_value" location')
    unresolvedRefInput = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn rawName) varName
    output = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (Fqn rawName) varName
    unresolvedRef = Bitcode.UnresolvedRef (Bitcode.UnresolvedRefContent output unresolvedRefInput location')
    instruction = Bitcode.Instruction location' unresolvedRef
    cfg = Cfg.atom (Cfg.Node instruction)
    in GeneratedExp cfg output (ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent "KOKO77777"))

someType :: ActualType
someType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent "KOKO77777")

-- | for some reason, the variable needed to generate an exp
-- does not exist in the symbol table. this is (probably? surely?) an error.
--
-- EDIT: This is NOT an error - it is perfectly fine ...
--
-- best thing we can do is use some universal variable to capture all
-- the missing variables
codeGenExpVarSimpleMissing :: Token.VarName -> GeneratedExp
codeGenExpVarSimpleMissing v = let
    generatedExp = nondet v
    content' = ActualType.ThirdPartyImportContent (Token.content (Token.getVarNameToken v))
    actualType = ActualType.ThirdPartyImport content'
    in generatedExp { inferredActualType = actualType }

codeGenExpVarSimpleExisting :: Token.VarName -> Bitcode.Variable -> ActualType -> GeneratedExp
codeGenExpVarSimpleExisting v b t = GeneratedExp (Cfg.empty (Token.getVarNameLocation v)) b t 

codeGenVarSimple :: Ast.VarSimpleContent -> CodeGenState -> GeneratedExp
codeGenVarSimple v ctx = case SymbolTable.lookupVar (Ast.varName v) (symbolTable ctx) of
    Nothing -> codeGenExpVarSimpleMissing (Ast.varName v)
    Just (bitcodeVar, actualType) -> codeGenExpVarSimpleExisting (Ast.varName v) bitcodeVar actualType
 
codeGenVarField :: Ast.VarFieldContent -> CodeGenContext GeneratedExp
codeGenVarField v = do
    v' <- codeGenExp (Ast.varFieldLhs v)
    let fieldName = Ast.varFieldName v
    let cfgLhsExpVar = generatedCfg v'
    let input = generatedValue v'
    let actualType = inferredActualType v'
    let actualType' = ActualType.getFieldedAccess actualType fieldName
    let outputFqn = ActualType.toFqn actualType'
    let location' = Ast.varFieldLocation v
    let output = Bitcode.TmpVariableCtor (Bitcode.TmpVariable outputFqn location')
    let fieldReadContent = Bitcode.FieldReadContent output input fieldName
    let fieldRead = Bitcode.FieldRead fieldReadContent
    let fieldReadInstruction = Bitcode.Instruction location' fieldRead
    let cfgFieldRead = Cfg.atom (Cfg.Node fieldReadInstruction)
    let cfg = cfgLhsExpVar `Cfg.concat` cfgFieldRead
    return $ GeneratedExp cfg output actualType'

codeGenVarSubscript :: Ast.VarSubscriptContent -> CodeGenContext GeneratedExp
codeGenVarSubscript v = do
    v' <- codeGenExp (Ast.varSubscriptLhs v)
    i' <- codeGenExp (Ast.varSubscriptIdx v)
    let cfgLhs = generatedCfg v'
    let cfgIdx = generatedCfg i'
    let input = generatedValue v'
    let index = generatedValue i'
    let actualType = inferredActualType v'
    let outputFqn = ActualType.toFqn actualType
    let location' = Ast.varSubscriptLocation v
    let output = Bitcode.TmpVariableCtor (Bitcode.TmpVariable outputFqn location')
    let subscriptReadContent = Bitcode.SubscriptReadContent output input index
    let subscriptRead = Bitcode.SubscriptRead subscriptReadContent
    let subscriptReadInstruction = Bitcode.Instruction location' subscriptRead
    let cfgSubscriptRead = Cfg.atom (Cfg.Node subscriptReadInstruction)
    let cfg = cfgIdx `Cfg.concat` cfgLhs `Cfg.concat` cfgSubscriptRead
    return $ GeneratedExp cfg output actualType

codeGenVar :: Ast.Var -> CodeGenContext GeneratedExp
codeGenVar (Ast.VarSimple    v) = do { ctx <- get; return $ codeGenVarSimple v ctx }
codeGenVar (Ast.VarField     v) = codeGenVarField v
codeGenVar (Ast.VarSubscript v) = codeGenVarSubscript v

-- | dispatch codegen (exp) var handlers
codeGenExpVar :: Ast.ExpVarContent -> CodeGenContext GeneratedExp
codeGenExpVar (Ast.ExpVarContent v) = codeGenVar v

-- | code gen exps ( plural )
codeGenExps :: [ Ast.Exp ] -> CodeGenContext [ GeneratedExp ] 
codeGenExps = mapM codeGenExp

thirdPartyContent :: String -> ActualType
thirdPartyContent = ActualType.ThirdPartyImport . ActualType.ThirdPartyImportContent

-- | handle special javascript call: `require`
getReturnActualType' :: Args -> ActualType
getReturnActualType' [(GeneratedExp _ _ (NativeTypeConstStr value))] = thirdPartyContent ("npm." ++ value)
getReturnActualType' _ = someType

-- | normal case currently ignores overloading
getReturnActualType'' :: Callee -> ActualType
getReturnActualType'' (GeneratedExp _ _ (ActualType.Function f)) = (ActualType.returnType f)
getReturnActualType'' (GeneratedExp _ _ (ActualType.ThirdPartyImport i)) = (ActualType.ThirdPartyImport i)
getReturnActualType'' (GeneratedExp _ _ _) = ActualType.ThirdPartyImport (ThirdPartyImportContent "MOMO66")

-- | separate javascript `require` calls
getReturnActualType :: Callee -> Args -> ActualType
getReturnActualType (GeneratedExp _ _ ActualType.Require) args = getReturnActualType' args
getReturnActualType callee _ = getReturnActualType'' callee

buildTheActualCall' :: Callee -> Args -> Bitcode.Variable -> Location -> Bitcode.CallContent
buildTheActualCall' callee args output location' = let
    callee' = generatedValue callee
    args' = Data.List.map generatedValue args
    in Bitcode.CallContent output callee' args' location'

buildTheActualCall :: Callee -> Args -> Bitcode.Variable -> Location -> Cfg
buildTheActualCall c a v l = Cfg.atom $ Cfg.Node $ (Bitcode.Instruction l) $ Bitcode.Call $ buildTheActualCall' c a v l

codeGenExpCall' :: Callee -> Args -> Location -> GeneratedExp
codeGenExpCall' callee args location' = let
    returnType = getReturnActualType callee args
    output = Bitcode.TmpVariableCtor $ Bitcode.TmpVariable (ActualType.toFqn returnType) location'
    actualCall = buildTheActualCall callee args output location'
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
    symbolTable' = SymbolTable.insertVar (Ast.varName v) bitcodeVar someType (symbolTable ctx)
    in case bitcodeVarExists of { True -> ctx; False -> ctx { symbolTable = symbolTable' } }
}

-- |
--
-- * dynamic languages often have variable declarations
-- "hide" in plain assignment syntax - this means that
-- assignment scanned according to their "ast order"
-- will insert the assigned variable to the symbol table
-- (if it hasn't been inserted before)
--
-- * this is /very/ similar to `codeGenDecVarInit`
--
codeGenStmtAssignToSimpleVar :: Token.VarName -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToSimpleVar varName init = do
    init' <- codeGenExp init
    ctx <- get;
    let initCfg = generatedCfg init'
    let initVariable = generatedValue init'
    let actualType = inferredActualType init'
    let fqn = ActualType.toFqn actualType
    let location' = Token.getVarNameLocation varName
    let existingVar = SymbolTable.lookupVar varName (symbolTable ctx)
    let newVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let symbolTable' = SymbolTable.insertVar varName newVariable actualType (symbolTable ctx)
    let updateSymbolTable = ctx { symbolTable = symbolTable' }
    let dontChangeCtx = ctx
    let actualVar = case existingVar of { Nothing -> newVariable; Just (v, _) -> v }
    put $ case existingVar of { Nothing -> updateSymbolTable; _ -> dontChangeCtx }
    return $ initCfg `Cfg.concat` (createAssignCfg location' actualVar initVariable)
    
codeGenStmtAssignToFieldVar :: Ast.VarFieldContent -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToFieldVar var exp = do
    exp' <- codeGenExp exp
    var' <- codeGenExp (Ast.varFieldLhs var)
    let location' = Ast.varFieldLocation var
    let lhsVar = generatedValue var'
    let expVar = generatedValue exp'
    let fieldName = Ast.varFieldName var
    let content' = Bitcode.FieldWriteContent lhsVar fieldName expVar
    let fieldWrite = Bitcode.FieldWrite content'
    let instruction = Bitcode.Instruction location' fieldWrite
    let expCfg = generatedCfg exp'
    let varCfg = generatedCfg var'
    let cfg = Cfg.atom (Cfg.Node instruction)
    return $ expCfg `Cfg.concat` varCfg `Cfg.concat` cfg

codeGenStmtAssignToSubscriptVar :: Ast.VarSubscriptContent -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToSubscriptVar subscriptVar value = do
    value' <- codeGenExp value
    index <- codeGenExp (Ast.varSubscriptIdx subscriptVar)
    lhs <- codeGenExp (Ast.varSubscriptLhs subscriptVar)
    let valueCfg = generatedCfg value'
    let indexCfg = generatedCfg index
    let lhsCfg = generatedCfg lhs
    let lhsType = inferredActualType lhs
    let fqn = ActualType.toFqn lhsType
    let location' = Ast.varSubscriptLocation subscriptVar
    let varName = Token.VarName $ Token.Named (Fqn.content fqn) location'
    let output = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let subscriptIdx = generatedValue index
    let input = generatedValue value'
    let content' = Bitcode.SubscriptWriteContent output subscriptIdx input
    let subscriptWrite = Bitcode.SubscriptWrite content'
    let instruction = Bitcode.Instruction location' subscriptWrite
    let cfg = valueCfg `Cfg.concat` indexCfg `Cfg.concat` lhsCfg 
    return $ cfg `Cfg.concat` (Cfg.atom (Cfg.Node instruction))

codeGenStmtAssign' :: Ast.Var -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssign' (Ast.VarSimple    v) e = codeGenStmtAssignToSimpleVar (Ast.varName v) e
codeGenStmtAssign' (Ast.VarField     v) e = codeGenStmtAssignToFieldVar v e
codeGenStmtAssign' (Ast.VarSubscript v) e = codeGenStmtAssignToSubscriptVar v e

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
codeGenStmtDecvar :: Ast.StmtVardecContent -> CodeGenContext Cfg
codeGenStmtDecvar d = case Ast.stmtVardecInitValue d of
    Nothing -> codeGenDecVarNoInit d (Token.getVarNameLocation (Ast.stmtVardecName d))
    Just init -> codeGenDecVarInit (Ast.stmtVardecName d) (Ast.stmtVardecNominalType d) init

-- |
-- * update the symbol table with the declared variable
--
-- * return an empty cfg (nop) since nothing happens (no init)
codeGenDecVarNoInit :: Ast.StmtVardecContent -> Location -> CodeGenContext Cfg
codeGenDecVarNoInit d loc = do { codeGenDecVarNoInit' d; return $ Cfg.empty loc }

-- | helper for codeGenDecVarInit
createAssignCfg :: Location -> Bitcode.Variable -> Bitcode.Variable -> Cfg
createAssignCfg location' srcVariable initVariable = let
    assignContent = Bitcode.AssignContent srcVariable initVariable
    assign = Bitcode.Assign assignContent
    instruction = Bitcode.Instruction location' assign
    in Cfg.atom (Cfg.Node instruction)

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
codeGenDecVarInit varName _ init = do
    init' <- codeGenExp init
    ctx <- get
    let initCfg = generatedCfg init'
    let initVariable = generatedValue init'
    let actualType = inferredActualType init'
    let fqn = ActualType.toFqn actualType
    let location' = Token.getVarNameLocation varName
    let srcVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let symbolTable' = SymbolTable.insertVar varName srcVariable actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' } -- finished updating the state
    return $ initCfg `Cfg.concat` (createAssignCfg location' srcVariable initVariable)

-- | pure non-monadic function, just update
-- the symbol table with the declared variable
codeGenDecVarNoInit' :: Ast.StmtVardecContent -> CodeGenContext ()
codeGenDecVarNoInit' vardec = do
    ctx <- get;
    let varName = Ast.stmtVardecName vardec
    let nominalType = Ast.stmtVardecNominalType vardec
    let actualType = SymbolTable.lookupNominalType nominalType (symbolTable ctx)
    let bitcodeVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (ActualType.toFqn actualType) varName
    let symbolTable' = SymbolTable.insertVar varName bitcodeVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }

codeGenExpInt :: Ast.ExpIntContent -> GeneratedExp
codeGenExpInt expInt = let
    constInt = Ast.expIntValue expInt
    constIntValue = Token.constIntValue constInt
    location' = Token.constIntLocation constInt
    tmpVariable = Bitcode.TmpVariable Fqn.nativeInt location'
    variable = Bitcode.TmpVariableCtor tmpVariable
    loadImmInt = Bitcode.IntContent tmpVariable constInt
    instruction = Bitcode.Instruction location' $ Bitcode.LoadImmInt loadImmInt
    actualType = ActualType.NativeTypeConstInt constIntValue
    in GeneratedExp (Cfg.atom $ Node instruction) variable actualType

codeGenExpStr :: Ast.ExpStrContent -> GeneratedExp
codeGenExpStr expStr = let
    constStr = Ast.expStrValue expStr
    constStrValue = Token.constStrValue constStr
    location' = Token.constStrLocation constStr
    tmpVariable = Bitcode.TmpVariable Fqn.nativeStr location'
    variable = Bitcode.TmpVariableCtor tmpVariable
    loadImmStr = Bitcode.StrContent tmpVariable constStr
    instruction = Bitcode.Instruction location' $ Bitcode.LoadImmStr loadImmStr
    actualType = ActualType.NativeTypeConstStr constStrValue
    in GeneratedExp (Cfg.atom $ Node instruction) variable actualType

-- minor non interesting helper functions here

scriptToCallable :: Cfg -> Callable
scriptToCallable cfg = let
    filename' = Location.filename (Cfg.location cfg)
    content' = Callable.ScriptContent filename' cfg
    in Callable.Script content'

decFuncToCallable :: Ast.StmtFuncContent -> Cfg -> [ Callable.Annotation ] -> Callable
decFuncToCallable stmtFunc cfg annotations = let
    content' = Callable.FunctionContent (Ast.stmtFuncName stmtFunc) cfg annotations (Ast.stmtFuncLocation stmtFunc)
    in Callable.Function content'

fqnify :: SymbolTable -> [ Token.SuperName ] -> [ Fqn ]
fqnify symbolTable = Data.List.map (fqnify' symbolTable)

fqnify' :: SymbolTable -> Token.SuperName -> Fqn
fqnify' symbolTable superName = let
    actualType = SymbolTable.lookupSuperType superName symbolTable
    in case actualType of
        Nothing -> Fqn (Token.content (Token.getSuperNameToken superName))
        Just actualType' -> toFqn actualType'

stmtMethodToCallable :: Ast.StmtMethodContent -> SymbolTable -> Cfg -> Callable
stmtMethodToCallable stmtMethod symbolTable cfg = let
    hostingClassName' = Ast.hostingClassName stmtMethod
    hostingClassSupers' = fqnify symbolTable (Ast.hostingClassSupers stmtMethod)
    stmtMethodName' = Ast.stmtMethodName stmtMethod
    location' = Ast.stmtMethodLocation stmtMethod
    content' = Callable.MethodContent stmtMethodName' hostingClassName' hostingClassSupers' cfg location'
    in Callable.Method content'

lambdaToCallable :: Cfg -> Location -> Callable
lambdaToCallable cfg location' = let
    content' = Callable.LambdaContent cfg location'
    in Callable.Lambda content'

-- | Temporarily
defaultLoc :: Location
defaultLoc = Location "" 0 0 0 0

