{-|
Module          : Language.Snobol4.VM.Bytecode.Compiler.Simple
Description     : Simple Implementation of the Bytecode Compiler
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.VM.Bytecode.Compiler.Simple where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Control.Monad.State

import Language.Snobol4.Syntax.AST

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.VM.Bytecode
import Language.Snobol4.VM.Bytecode.Compiler
--import Language.Snobol4.VM.Bytecode.Primitives

-- | Result of compilation
data CompilerResult
    = 
    -- | Failure with errors and the address of their origin
      CompileFailed [(Address, CompilerError)]
    -- | Success with instructions and symbol table
    | CompileSucceeded CompiledProgram SymbolTable
  deriving Show

-- | State of the compiler
data SimpleCompilerState
    = SimpleCompilerState
    { 
    -- | Instructions generated so far
      instructions :: Vector Instruction
    -- | Table of symbols found
    , symbolTable :: SymbolTable
    -- | Errors encountered
    , compilerErrors :: Vector (Address, CompilerError)
    }    

-- | Monad for managing compiler state
newtype SimpleCompiler a = SimpleCompiler
    { runSimpleCompilerInternal :: State SimpleCompilerState a }
  deriving (Functor, Applicative, Monad)

-- | Compiler with no instructions, error, or symbols
emptyCompilerState :: SimpleCompilerState
emptyCompilerState
    = SimpleCompilerState
      V.empty
      emptySymbolTable
      V.empty

-- | Generate bytecode, symbols, and errors from an AST
simpleCompiler :: Program -> CompilerResult
simpleCompiler prog
    | V.null $ compilerErrors finalState
        = CompileSucceeded
        ( CompiledProgram $ instructions finalState )
        ( symbolTable finalState )
    | otherwise = CompileFailed $ V.toList $ compilerErrors finalState
  where
    finalState = flip execState emptyCompilerState $ do
        runSimpleCompilerInternal $ do
            compile prog
            
-- | Get the maximum key of a map    
maxKey :: Ord k => Map k a -> Maybe k
maxKey m = case M.keys m of
    [] -> Nothing
    ks -> Just $ maximum ks

-- | Get the maximum value of a map
maxElem :: (Ord k, Ord a) => Map k a -> Maybe a
maxElem m = case M.elems m of
    [] -> Nothing
    es -> Just $ maximum es

-- | Get the address of the next instruction to add
getCurrentAddress :: SimpleCompiler Address
getCurrentAddress 
    = SimpleCompiler 
    $ liftM (Address . mkInteger . V.length) 
    $ gets instructions

-- | Get the next availible id for compiler-generated labels
nextAvailibleSystemLabel :: SimpleCompiler SystemLabel
nextAvailibleSystemLabel
    = SimpleCompiler 
    $ liftM (maybe 0 succ . maxKey) 
    $ gets (systemLabels . symbolTable)

{-
nextAvailibleVarSymbol :: SimpleCompiler Symbol
nextAvailibleVarSymbol
    = SimpleCompiler 
    $ liftM (maybe 0 succ . maxElem)
    $ gets (varSymbols . symbolTable)
-}

{-
nextAvailibleFuncSymbol :: SimpleCompiler Symbol
nextAvailibleFuncSymbol
    = SimpleCompiler 
    $ liftM (maybe 0 succ . maxElem) 
    $ gets (funcSymbols . symbolTable)
-}

-- | Apply a function to the compiler state
modifyCompilerState :: (SimpleCompilerState -> SimpleCompilerState) -> SimpleCompiler ()
modifyCompilerState = SimpleCompiler . modify

-- | Apply a function to the symbol table
modifySymbolTable :: (SymbolTable -> SymbolTable) -> SimpleCompiler ()
modifySymbolTable f = modifyCompilerState $ \st -> st{ symbolTable = f $ symbolTable st }

-- | Apply a function to the instructions
modifyInstructions :: (Vector Instruction -> Vector Instruction) -> SimpleCompiler ()
modifyInstructions f = modifyCompilerState $ \st -> st{ instructions = f $ instructions st }

-- | Apply a function to the collection of user-defined labels
modifyUserLabels :: (Map Snobol4String Address -> Map Snobol4String Address) -> SimpleCompiler ()
modifyUserLabels f = modifySymbolTable $ \st -> st{ userLabels = f $ userLabels st }

-- | Apply a function to the collection of compiler-generated labels
modifySystemLabels :: (Map SystemLabel (Maybe Address) 
                   -> Map SystemLabel (Maybe Address)) 
                   -> SimpleCompiler ()
modifySystemLabels f = modifySymbolTable $ \st -> st{ systemLabels = f $ systemLabels st }

-- | Apply a function to the collection of variable symbols
modifyVarSymbols :: (Map Snobol4String Symbol -> Map Snobol4String Symbol) -> SimpleCompiler ()
modifyVarSymbols f = modifySymbolTable $ \st -> st{ varSymbols = f $ varSymbols st }

-- | Apply a function to the collection of function symbols
modifyFuncSymbols :: (Map Snobol4String Symbol -> Map Snobol4String Symbol) -> SimpleCompiler ()
modifyFuncSymbols f = modifySymbolTable $ \st -> st{ funcSymbols = f $ funcSymbols st }

-- | Apply a function to the collection of errors
modifyCompilerErrors :: (Vector (Address, CompilerError) -> Vector (Address, CompilerError))
                     -> SimpleCompiler ()
modifyCompilerErrors f = modifyCompilerState $ \st -> st{ compilerErrors = f $ compilerErrors st }

-- | Apply a function to the entry point
modifyEntryPoint :: (Address -> Address) -> SimpleCompiler ()
modifyEntryPoint f = modifySymbolTable $ \st -> st{ programEntryPoint = f $ programEntryPoint st }

-- | Use the stateful monad to manage instructions, etc
instance Compiler SimpleCompiler where
    addUserLabel lbl = do
        addr <- getCurrentAddress
        modifyUserLabels $ M.insert lbl addr
    getUserLabel lbl = do
        SimpleCompiler $ gets $ M.lookup lbl . userLabels . symbolTable
    allocSystemLabel = do
        lbl <- nextAvailibleSystemLabel
        modifySystemLabels $ M.insert lbl Nothing
        return lbl
    addSystemLabel lbl = do
        addr <- getCurrentAddress
        modifySystemLabels $ M.insert lbl $ Just addr
    addInstruction = modifyInstructions . flip V.snoc
    getVarSymbol ident = do
        result <- SimpleCompiler $ liftM (M.lookup ident) $ gets $ (varSymbols . symbolTable)
        case result of
            Nothing -> do
                --sym <- nextAvailibleVarSymbol
                let sym = Symbol ident
                modifyVarSymbols $ M.insert ident sym
                return sym
            Just sym -> return sym
    getFuncSymbol ident = do
        result <- SimpleCompiler $ liftM (M.lookup ident) $ gets $ (funcSymbols . symbolTable)
        case result of
            Nothing -> do
                --sym <- nextAvailibleFuncSymbol
                let sym = Symbol ident
                modifyFuncSymbols $ M.insert ident sym
                return sym
            Just sym -> return sym
    compileError err = do
        addr <- getCurrentAddress
        modifyCompilerErrors $ flip V.snoc (addr, err)
    getPanicLabel = return $ -1
    setEntryPoint addr = modifyEntryPoint $ const addr
    getSystemLabelAddress lbl = SimpleCompiler $ gets $ join . M.lookup lbl . systemLabels . symbolTable
        
