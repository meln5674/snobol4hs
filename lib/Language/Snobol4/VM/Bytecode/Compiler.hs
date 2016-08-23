module Language.Snobol4.VM.Bytecode.Compiler where

import Control.Monad

import Language.Snobol4.Syntax.AST
import Language.Snobol4.VM.Bytecode

import Language.Snobol4.Interpreter.Data

data CompilerError
    = LiteralAsLValue
  deriving Show

class Monad m => Compiler m where
    addUserLabel :: Snobol4String -> m ()
    allocSystemLabel :: m SystemLabel
    addSystemLabel :: SystemLabel -> m ()
    addInstruction :: Instruction -> m ()
    getVarSymbol :: Snobol4String -> m Symbol
    getFuncSymbol :: Snobol4String -> m Symbol
    compileError :: CompilerError -> m ()
    getPanicLabel :: m SystemLabel

data LValue
    = StaticLValue Symbol
    | StaticRefLValue Symbol Int
    | DynamicLValue

compile :: Compiler m => Program -> m ()
compile = mapM_ compileStatement . getProgram

compileStatement :: Compiler m => Stmt -> m ()
compileStatement (EndStmt _) = return ()
compileStatement stmt = do
    lbl <- allocSystemLabel
    compileStatementLabel stmt
    addInstruction $ SetFailLabel lbl
    compileStatementBody stmt
    compileStatementGoto lbl stmt

compileStatementLabel :: Compiler m => Stmt -> m ()
compileStatementLabel stmt@(Stmt Nothing _ _ _ _) = return ()
compileStatementLabel stmt@(Stmt (Just lbl) _ _ _ _) = addUserLabel $ mkString lbl

compileStatementBody :: Compiler m => Stmt -> m ()
compileStatementBody (Stmt _ Nothing _ _ _) = return ()
compileStatementBody (Stmt _ (Just sub) Nothing Nothing _) = do
    lvalue <- compileSubject sub
    case lvalue of
        StaticLValue _ -> return ()
        StaticRefLValue _ argCount -> replicateM_ argCount $ addInstruction Pop
        DynamicLValue -> addInstruction Pop
compileStatementBody (Stmt _ (Just sub) (Just pat) Nothing _) = do
    lvalue <- compileSubject sub
    case lvalue of
        StaticLValue sym -> do
            addInstruction $ LookupStatic sym
        StaticRefLValue sym argCount -> do
            addInstruction $ RefStatic sym argCount
        DynamicLValue -> return ()
    compilePattern pat
    addInstruction $ InvokeScanner
    addInstruction $ Pop    
    addInstruction $ Pop    
compileStatementBody (Stmt _ (Just sub) Nothing (Just obj) _) = do
    lvalue <- compileSubject sub
    compileObject obj
    case lvalue of
        StaticLValue sym -> addInstruction $ AssignStatic sym
        StaticRefLValue sym argCount -> addInstruction $ AssignRefStatic sym argCount
        DynamicLValue -> addInstruction AssignDynamic
compileStatementBody (Stmt _ (Just sub) (Just pat) (Just obj) _) = do
    lvalue <- compileSubject sub
    case lvalue of
        StaticLValue sym -> do
            addInstruction $ Copy 1
            addInstruction $ LookupStatic sym
        StaticRefLValue sym argCount -> do
            addInstruction $ Copy argCount
            addInstruction $ RefStatic sym argCount
        DynamicLValue -> return ()
    compilePattern pat
    addInstruction $ InvokeScanner
    addInstruction $ InvokeReplacer


compileGotoPart :: Compiler m => GotoPart -> m ()
compileGotoPart (GotoPart expr) = do
    compileGotoValue expr
    addInstruction $ JumpDynamic
compileGotoPart (DirectGotoPart expr) = do
    compileGotoValue expr
    addInstruction $ DirectJump

compileStatementGoto :: Compiler m => SystemLabel -> Stmt -> m ()
compileStatementGoto lbl (Stmt _ _ _ _ Nothing) = addSystemLabel lbl
compileStatementGoto lbl (Stmt _ _ _ _ (Just (Goto part))) = do
    panicLbl <- getPanicLabel
    addSystemLabel lbl
    addInstruction $ SetFailLabel panicLbl
    compileGotoPart part    
compileStatementGoto lbl (Stmt _ _ _ _ (Just (SuccessGoto part))) = do
    panicLbl <- getPanicLabel
    addInstruction $ SetFailLabel panicLbl
    compileGotoPart part    
    addSystemLabel lbl
compileStatementGoto lbl (Stmt _ _ _ _ (Just (FailGoto part))) = do
    successLbl <- allocSystemLabel
    addInstruction $ JumpStatic successLbl
    panicLbl <- getPanicLabel
    addSystemLabel lbl
    addInstruction $ SetFailLabel panicLbl
    compileGotoPart part
    addSystemLabel successLbl
compileStatementGoto lbl (Stmt _ _ _ _ (Just (BothGoto success failure))) = do
    panicLbl <- getPanicLabel
    addInstruction $ SetFailLabel panicLbl
    compileGotoPart success
    addSystemLabel lbl
    addInstruction $ SetFailLabel panicLbl
    compileGotoPart failure

compileSubject :: Compiler m => Expr -> m LValue
compileSubject = compileLValue

compilePattern :: Compiler m => Expr -> m ()
compilePattern = compileRValue

compileObject :: Compiler m => Expr -> m ()
compileObject = compileRValue

compileRValue :: Compiler m => Expr -> m ()
compileRValue (PrefixExpr op expr) = do
    compileRValue expr
    addInstruction $ UnOp op
compileRValue (IdExpr name) = do
    sym <- getVarSymbol $ mkString name
    addInstruction $ LookupStatic sym
compileRValue (LitExpr (Int i)) = do
    addInstruction $ Push $ IntegerData $ mkInteger i
compileRValue (LitExpr (Real r)) = do
    addInstruction $ Push $ RealData $ mkReal r
compileRValue (LitExpr (String s)) = do
    addInstruction $ Push $ StringData $ mkString s
compileRValue (CallExpr name argExprs) = do
    sym <- getFuncSymbol $ mkString name
    mapM compileRValue $ reverse argExprs
    addInstruction $ CallStatic sym $ length argExprs
compileRValue (RefExpr name argExprs) = do
    sym <- getVarSymbol $ mkString name
    mapM compileRValue $ reverse argExprs
    addInstruction $ RefStatic sym $ length argExprs
compileRValue (ParenExpr expr) = compileRValue expr
compileRValue (BinaryExpr expr1 op expr2) = do
    compileRValue expr1
    compileRValue expr2
    addInstruction $ BinOp op
compileRValue NullExpr = do
    addInstruction $ Push $ StringData $ nullString



compileLValue :: Compiler m => Expr -> m LValue
compileLValue (PrefixExpr op expr) = do
    compileRValue expr
    addInstruction $ UnOp op
    return DynamicLValue
compileLValue (IdExpr name) = do
    sym <- getVarSymbol $ mkString name
    return $ StaticLValue sym
compileLValue (LitExpr _) = do
    compileError LiteralAsLValue
    return DynamicLValue
compileLValue expr@CallExpr{} = do
    compileRValue expr
    return DynamicLValue
compileLValue (RefExpr name argExprs) = do
    sym <- getVarSymbol $ mkString name
    mapM compileRValue $ reverse argExprs
    return $ StaticRefLValue sym $ length argExprs
compileLValue (ParenExpr expr) = do
    compileRValue expr
    return DynamicLValue
compileLValue expr@BinaryExpr{} = do
    compileRValue expr
    return DynamicLValue
compileLValue NullExpr = do
    compileError LiteralAsLValue
    return DynamicLValue

compileGotoValue :: Compiler m => Expr -> m ()
compileGotoValue (IdExpr name) = addInstruction $ Push $ StringData $ mkString name
compileGotoValue expr = compileRValue expr
