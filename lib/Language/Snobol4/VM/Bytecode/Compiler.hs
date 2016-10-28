{-|
Module          : Language.Snobol4.VM.Bytecode.Compiler
Description     : Bytecode compiler
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

This module provides functions which only control how instructions, labels, and
    symbols are generated, and in what order, along w. The Compiler typeclass is
    used to control how these are managed.
-}

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

import Language.Snobol4.Interpreter.Internal.StateMachine hiding (Return, FReturn, getProgram)

-- | A compiler error
data CompilerError
    = 
    -- | The end label does not refer to a known label
      BadEndLabel
    -- | An illegal L-value was found, such as a literal
    | IllegalLValue
  deriving Show

-- | Class of monads which manage compiler details
class Monad m => Compiler m where
    -- | Register a new user-defined label at the current address
    addUserLabel :: Snobol4String -> m ()
    -- | Retreive the address of a user-defined label
    getUserLabel :: Snobol4String -> m (Maybe Address)
    -- | Allocate a new compiler-generated label with no defined address
    allocSystemLabel :: m SystemLabel
    -- | Set the address of a compiler-generated label
    addSystemLabel :: SystemLabel -> m ()
    -- | Add an instruction
    addInstruction :: Instruction -> m ()
    -- | Get the symbol to use for a variable
    getVarSymbol :: Snobol4String -> m Symbol
    -- | Get the symbol to use for a function
    getFuncSymbol :: Snobol4String -> m Symbol
    -- | Add an error
    compileError :: CompilerError -> m ()
    -- | Get the label to jump to for error termination
    getPanicLabel :: m SystemLabel
    -- | Set the entry point of the program
    setEntryPoint :: Address -> m ()
    -- | Get the address of a compiler-generated label
    getSystemLabelAddress :: SystemLabel -> m (Maybe Address)

-- | An L-value, used to determine how to assign a value
data LValue
    = 
    -- | Assignment to a static variable
      StaticLValue Symbol
    -- | Assignment to a keyword
    | StaticKeywordLValue Symbol
    -- | Assignment to a static aggregate (array/table)
    | StaticRefLValue Symbol Int
    -- | Assignment to a dynamically computed reference
    | DynamicLValue
    -- | Assignment to the INPUT variable
    | InputLValue
    -- | Assignment to the OUTPUT variable
    | OutputLValue
    -- | Assignment to the PUNCH variable
    | PunchLValue

-- | Generate bytecode from an AST
compile :: Compiler m => Program -> m ()
compile prog = do
    mapM_ compileStatement $ getProgram prog
    addUserLabel "END"
    addInstruction Finish

-- | Generate bytecode for a statement
compileStatement :: Compiler m => Stmt -> m ()
compileStatement (EndStmt Nothing) = return ()
compileStatement (EndStmt (Just lbl)) = do
    labelAddr <- getUserLabel $ mkString lbl
    case labelAddr of
        Just addr -> setEntryPoint addr
        Nothing -> compileError BadEndLabel
compileStatement stmt = do
    lbl <- allocSystemLabel
    compileStatementLabel stmt
    addInstruction $ SetFailLabel lbl
    compileStatementBody stmt
    compileStatementGoto lbl stmt

-- | Generate bytecode/labels for a statement label
compileStatementLabel :: Compiler m => Stmt -> m ()
compileStatementLabel (Stmt Nothing _ _ _ _) = return ()
compileStatementLabel (Stmt (Just lbl) _ _ _ _) = addUserLabel $ mkString lbl
compileStatementLabel (EndStmt _) = undefined

-- | Generate bytecode for a statement's body (subject, pattern, object)
compileStatementBody :: Compiler m => Stmt -> m ()
compileStatementBody (Stmt _ Nothing _ _ _) = return ()
compileStatementBody (Stmt _ (Just sub) Nothing Nothing _) = do
    compileRValue sub
    addInstruction Pop
    {-
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
        -}
compileStatementBody (Stmt _ (Just sub) (Just pat) Nothing _) = do
    compileRValue sub
    {-
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
    -}
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
    
-- | Generate bytecode for a evaluating and jumping to a goto
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

-- | Generate bytecode for a statement's goto
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

-- | Generate bytecode for a statement's subject (left side of assignment)
compileSubject :: Compiler m => Expr -> m LValue
compileSubject = compileLValue

-- | Generate bytecode for a statement's pattern
compilePattern :: Compiler m => Expr -> m ()
compilePattern = compileRValue

-- | Generate bytecode for a statement's object (right side of assignment)
compileObject :: Compiler m => Expr -> m ()
compileObject = compileRValue

-- | Generate bytecode for evaluating an R-value.
-- After running this code, the result of the expression is on the stack.
compileRValue :: Compiler m => Expr -> m ()
compileRValue (PrefixExpr And (IdExpr sym)) = do
    addInstruction $ LookupStaticKeyword $ Symbol $ mkString sym
compileRValue (PrefixExpr Star expr) = do
    exprLabel <- allocSystemLabel
    afterLabel <- allocSystemLabel
    exprFailLabel <- allocSystemLabel
    
    addInstruction $ JumpStatic afterLabel
    
    addSystemLabel exprLabel
    addInstruction $ PushFailLabel exprFailLabel
    compileRValue expr
    addInstruction ExprReturn
    addSystemLabel exprFailLabel
    addInstruction ExprFReturn
    
    
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
compileRValue (PrefixExpr At expr) = do
    lvalue <- compileLValue expr
    case lvalue of
        StaticLValue (Symbol sym) -> addInstruction $ PushReference sym
        StaticKeywordLValue (Symbol sym) -> addInstruction $ PushReferenceKeyword sym
        StaticRefLValue (Symbol sym) count -> addInstruction $ PushReferenceAggregate sym $ mkInteger count
        DynamicLValue -> return ()
        InputLValue -> addInstruction $ PushReferenceInput
        OutputLValue -> addInstruction $ PushReferenceOutput
        PunchLValue -> addInstruction $ PushReferencePunch
    addInstruction $ UnOp At
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


-- | Generate bytecode for evaluating an L-value, and indicate what kind it is.
-- Static variables and keywords will produce no bytecode.
-- Static aggregates will produce the bytecode to evaluate their arguments, and
--  after running it, the values of their arguments will be on the stack.
-- Dynamic L-values will generate bytecode that, when run, will place a
--  reference on the stack.
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

-- | Generate the bytecode for evaluating a goto target
compileGotoValue :: Compiler m => Expr -> m ()
compileGotoValue (IdExpr name) = addInstruction $ PushString $ mkString name
compileGotoValue expr = compileRValue expr
