module Language.Snobol4.VM.Bytecode.Compiler where

import Control.Monad

import Language.Snobol4.Syntax.AST
import Language.Snobol4.VM.Bytecode

import Language.Snobol4.Interpreter.Data (Data(StringData,IntegerData,RealData))
import Language.Snobol4.Interpreter.Data.String
import Language.Snobol4.Interpreter.Data.Integer
import Language.Snobol4.Interpreter.Data.Real

data CompilerError
    = LiteralAsLValue
    | BadEndLabel
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
    setEntryPoint :: Snobol4String -> m ()

data LValue
    = StaticLValue Symbol
    | StaticRefLValue Symbol Int
    | DynamicLValue
    | InputLValue
    | OutputLValue
    | PunchLValue

compile :: Compiler m => Program -> m ()
compile prog = do
    mapM_ compileStatement $ getProgram prog
    addInstruction Finish

compileStatement :: Compiler m => Stmt -> m ()
compileStatement (EndStmt Nothing) = return ()
compileStatement (EndStmt (Just lbl)) = setEntryPoint $ mkString lbl
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
        StaticLValue sym -> do
            addInstruction $ LookupStatic sym
            addInstruction $ Pop
        StaticRefLValue _ argCount -> replicateM_ argCount $ addInstruction Pop
        DynamicLValue -> addInstruction Pop
        InputLValue -> do
            addInstruction Input
            addInstruction Pop
        OutputLValue -> do
            addInstruction LastOutput
            addInstruction Pop
        PunchLValue -> do
            addInstruction LastPunch
            addInstruction Pop
compileStatementBody (Stmt _ (Just sub) (Just pat) Nothing _) = do
    lvalue <- compileSubject sub
    case lvalue of
        StaticLValue sym -> do
            addInstruction $ LookupStatic sym
        StaticRefLValue sym argCount -> do
            addInstruction $ RefStatic sym argCount
        DynamicLValue -> return ()
        InputLValue -> addInstruction $ Input
        OutputLValue -> addInstruction $ LastOutput
        PunchLValue -> addInstruction $ LastPunch
    compilePattern pat
    addInstruction $ InvokeScanner
    addInstruction $ Pop
    addInstruction $ Pop
    addInstruction $ Pop
compileStatementBody (Stmt _ (Just sub) Nothing (Just obj) _) = do
    lvalue <- compileSubject sub
    compileObject obj
    case lvalue of
        StaticLValue sym -> addInstruction $ AssignStatic sym
        StaticRefLValue sym argCount -> addInstruction $ AssignRefStatic sym argCount
        DynamicLValue -> addInstruction AssignDynamic
        InputLValue -> addInstruction Pop
        OutputLValue -> addInstruction Output
        PunchLValue -> addInstruction Punch
compileStatementBody (Stmt _ (Just sub) (Just pat) (Just obj) _) = do
    lvalue <- compileSubject sub
    case lvalue of
        StaticLValue sym -> do
            addInstruction $ LookupStatic sym
        StaticRefLValue sym argCount -> do
            addInstruction $ Copy argCount
            addInstruction $ RefStatic sym argCount
        DynamicLValue -> addInstruction $ Copy 1
        InputLValue -> addInstruction Input
        OutputLValue -> addInstruction LastOutput
        PunchLValue -> addInstruction LastPunch
    compilePattern pat
    addInstruction $ InvokeScanner
    compileObject obj
    addInstruction $ InvokeReplacer
    case lvalue of
        StaticLValue sym -> addInstruction $ AssignStatic sym
        StaticRefLValue sym argCount -> addInstruction $ AssignRefStatic sym argCount
        DynamicLValue -> addInstruction AssignDynamic
        InputLValue -> addInstruction Pop
        OutputLValue -> addInstruction Output
        PunchLValue -> addInstruction Punch
    

compileGotoPart :: Compiler m => GotoPart -> m ()
compileGotoPart (GotoPart (IdExpr "RETURN")) = addInstruction Return
compileGotoPart (GotoPart (IdExpr "FRETURN")) = addInstruction FReturn
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
compileRValue (IdExpr "INPUT") = addInstruction Input
compileRValue (IdExpr "OUTPUT") = addInstruction LastOutput
compileRValue (IdExpr "PUNCH") = addInstruction LastPunch
compileRValue (IdExpr name) = do
    sym <- getVarSymbol $ mkString name
    addInstruction $ LookupStatic sym
compileRValue (LitExpr (Int i)) = do
    addInstruction $ PushInteger $ mkInteger i
compileRValue (LitExpr (Real r)) = do
    addInstruction $ PushReal $ mkReal r
compileRValue (LitExpr (String s)) = do
    addInstruction $ PushString $ mkString s
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
    addInstruction $ PushString nullString



compileLValue :: Compiler m => Expr -> m LValue
compileLValue (PrefixExpr op expr) = do
    compileRValue expr
    addInstruction $ UnOp op
    return DynamicLValue
compileLValue (IdExpr "INPUT") = return InputLValue
compileLValue (IdExpr "OUTPUT") = return OutputLValue
compileLValue (IdExpr "PUNCH") = return PunchLValue
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
compileGotoValue (IdExpr name) = addInstruction $ PushString $ mkString name
compileGotoValue expr = compileRValue expr
