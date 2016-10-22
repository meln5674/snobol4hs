{-# LANGUAGE OverloadedStrings #-}
module Language.Snobol4.VM.Bytecode.Compiler where

import qualified Data.Vector as V

import Control.Monad

import Language.Snobol4.Syntax.AST
import Language.Snobol4.VM.Bytecode

import Language.Snobol4.Interpreter.Data (Data(StringData,IntegerData,RealData), Lookup(..))
import Language.Snobol4.Interpreter.Data.String
import Language.Snobol4.Interpreter.Data.Integer
import Language.Snobol4.Interpreter.Data.Real

import Language.Snobol4.Interpreter.Internal.StateMachine hiding (Return, FReturn)

data CompilerError
    = BadEndLabel
    | IllegalLValue
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
    getSystemLabelAddress :: SystemLabel -> m Address

data LValue
    = StaticLValue Symbol
    | StaticKeywordLValue Symbol
    | StaticRefLValue Symbol Int
    | DynamicLValue
    | InputLValue
    | OutputLValue
    | PunchLValue

compile :: Compiler m => Program -> m ()
compile prog = do
    mapM_ compileStatement $ getProgram prog
    addUserLabel "END"
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
compileStatementLabel (Stmt Nothing _ _ _ _) = return ()
compileStatementLabel (Stmt (Just lbl) _ _ _ _) = addUserLabel $ mkString lbl
compileStatementLabel (EndStmt _) = undefined

compileStatementBody :: Compiler m => Stmt -> m ()
compileStatementBody (Stmt _ Nothing _ _ _) = return ()
compileStatementBody (Stmt _ (Just sub) Nothing Nothing _) = do
    lvalue <- compileSubject sub
    case lvalue of
        StaticLValue sym -> do
            addInstruction $ LookupStatic sym
            addInstruction $ Pop
        StaticRefLValue sym argCount -> do
            addInstruction $ LookupStaticRef sym argCount
            replicateM_ argCount $ addInstruction Pop
        StaticKeywordLValue sym -> do
            addInstruction $ LookupStaticKeyword sym
            addInstruction Pop
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
            addInstruction $ LookupStaticRef sym argCount
        StaticKeywordLValue sym -> addInstruction $ LookupStaticKeyword sym
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
        StaticKeywordLValue sym -> addInstruction $ AssignStaticKeyword sym
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
            addInstruction $ LookupStaticRef sym argCount
        StaticKeywordLValue sym -> addInstruction $ LookupStaticKeyword sym
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
        StaticKeywordLValue sym -> addInstruction $ AssignStaticKeyword sym
        DynamicLValue -> addInstruction AssignDynamic
        InputLValue -> addInstruction Pop
        OutputLValue -> addInstruction Output
        PunchLValue -> addInstruction Punch
compileStatementBody (EndStmt _) = undefined
    

compileGotoPart :: Compiler m => GotoPart -> m ()
compileGotoPart (GotoPart (IdExpr "RETURN")) = addInstruction Return
compileGotoPart (GotoPart (IdExpr "FRETURN")) = addInstruction FReturn
compileGotoPart (GotoPart (IdExpr "NRETURN")) = addInstruction NReturn
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
compileStatementGoto _ (EndStmt _) = undefined

compileSubject :: Compiler m => Expr -> m LValue
compileSubject = compileLValue

compilePattern :: Compiler m => Expr -> m ()
compilePattern = compileRValue

compileObject :: Compiler m => Expr -> m ()
compileObject = compileRValue

compileRValue :: Compiler m => Expr -> m ()
compileRValue (PrefixExpr And (IdExpr sym)) = do
    addInstruction $ LookupStaticKeyword $ Symbol $ mkString sym
compileRValue (PrefixExpr Star expr) = do
    exprLabel <- allocSystemLabel
    afterLabel <- allocSystemLabel
    addInstruction $ JumpStatic afterLabel
    addSystemLabel exprLabel
    compileRValue expr
    addInstruction $ ExprReturn
    addSystemLabel afterLabel
    addInstruction $ PushExpression exprLabel
compileRValue (PrefixExpr Not expr) = do
    operandFailLabel <- allocSystemLabel
    addInstruction $ PushFailLabel operandFailLabel
    compileRValue expr
    addInstruction $ Pop
    addInstruction $ PopFailLabel
    addInstruction $ JumpToFailureLabel
    addSystemLabel operandFailLabel
    addInstruction $ PopFailLabel
    addInstruction $ PushString nullString
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
    addInstruction $ CallStatic sym (length argExprs) False
compileRValue (RefExpr name argExprs) = do
    sym <- getVarSymbol $ mkString name
    mapM compileRValue $ reverse argExprs
    addInstruction $ LookupStaticRef sym $ length argExprs
compileRValue (ParenExpr expr) = compileRValue expr
compileRValue (BinaryExpr expr1 Dot expr2) = do
    lvalue <- compileLValue expr2
    case lvalue of
        StaticLValue (Symbol sym) -> addInstruction $ PushReference sym
        StaticKeywordLValue (Symbol sym) -> addInstruction $ PushReferenceKeyword sym
        StaticRefLValue (Symbol sym) count -> addInstruction $ PushReferenceAggregate sym $ mkInteger count
        DynamicLValue -> compileError IllegalLValue
        InputLValue -> addInstruction $ PushReferenceInput
        OutputLValue -> addInstruction $ PushReferenceOutput
        PunchLValue -> addInstruction $ PushReferencePunch
    compileRValue expr1
    addInstruction $ BinOp Dot
compileRValue (BinaryExpr expr1 Dollar expr2) = do
    lvalue <- compileLValue expr2
    case lvalue of
        StaticLValue (Symbol sym) -> addInstruction $ PushReference sym
        StaticKeywordLValue (Symbol sym) -> addInstruction $ PushReferenceKeyword sym
        StaticRefLValue (Symbol sym) count -> addInstruction $ PushReferenceAggregate sym $ mkInteger count
        DynamicLValue -> compileError IllegalLValue
        InputLValue -> addInstruction $ PushReferenceInput
        OutputLValue -> addInstruction $ PushReferenceOutput
        PunchLValue -> addInstruction $ PushReferencePunch
    compileRValue expr1
    addInstruction $ BinOp Dollar
compileRValue (BinaryExpr expr1 op expr2) = do
    compileRValue expr2
    compileRValue expr1
    addInstruction $ BinOp op
compileRValue NullExpr = do
    addInstruction $ PushString nullString



compileLValue :: Compiler m => Expr -> m LValue
compileLValue (PrefixExpr And (IdExpr sym)) = return $ StaticKeywordLValue $ Symbol $ mkString sym
compileLValue (PrefixExpr And (ParenExpr expr)) = compileLValue (PrefixExpr And expr)
compileLValue (PrefixExpr And _) = do
    compileError IllegalLValue
    return DynamicLValue
compileLValue (PrefixExpr Dollar expr) = do
    compileRValue expr
    addInstruction $ UnOp Dollar
    return DynamicLValue
{-
compileLValue (PrefixExpr op expr) = do
    lvalue <- compileLValue expr
    case lvalue of
        InputLValue -> addInstruction $ PushReference $ mkString "INPUT"
        OutputLValue -> addInstruction $ PushReference $ mkString "OUTPUT"
        PunchLValue -> addInstruction $ PushReference $ mkString "PUNCH"
        StaticLValue (Symbol sym) -> addInstruction $ PushReference sym
        StaticRefLValue (Symbol sym) i -> addInstruction $ PushReferenceAggregate sym (mkInteger i)
    addInstruction $ UnOp op
    return DynamicLValue
-}
compileLValue (PrefixExpr Not expr) = do
    compileRValue (PrefixExpr Not expr)
    return DynamicLValue
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
    compileError IllegalLValue
    return DynamicLValue
compileLValue (CallExpr name argExprs) = do
    sym <- getFuncSymbol $ mkString name
    mapM compileRValue $ reverse argExprs
    addInstruction $ CallStatic sym (length argExprs) True
    return DynamicLValue
compileLValue (RefExpr name argExprs) = do
    sym <- getVarSymbol $ mkString name
    mapM compileRValue $ reverse argExprs
    return $ StaticRefLValue sym $ length argExprs
compileLValue (ParenExpr expr) = do
    compileLValue expr
compileLValue (BinaryExpr expr1 op expr2) = do
    compileRValue expr2
    compileRValue expr1
    addInstruction $ BinOp op
    return $ DynamicLValue
compileLValue NullExpr = do
    compileError IllegalLValue
    return DynamicLValue

compileGotoValue :: Compiler m => Expr -> m ()
compileGotoValue (IdExpr name) = addInstruction $ PushString $ mkString name
compileGotoValue expr = compileRValue expr
