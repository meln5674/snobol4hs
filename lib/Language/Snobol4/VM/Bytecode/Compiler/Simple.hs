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
import Language.Snobol4.VM.Bytecode.Primitives

data CompilerResult
    = CompileFailed [(Address, CompilerError)]
    | CompileSucceeded CompiledProgram SymbolTable
  deriving Show

data SymbolTable
    = SymbolTable
    { userLabels :: Map Snobol4String Address
    , systemLabels :: Map SystemLabel (Maybe Address)
    , varSymbols :: Map Snobol4String Symbol
    , funcSymbols :: Map Snobol4String Symbol
    }
  deriving Show

emptySymbolTable :: SymbolTable
emptySymbolTable
    = SymbolTable
      M.empty
      M.empty
      M.empty
      M.empty


data SimpleCompilerState
    = SimpleCompilerState
    { instructions :: Vector Instruction
    , symbolTable :: SymbolTable
    , compilerErrors :: Vector (Address, CompilerError)
    }    

newtype SimpleCompiler a = SimpleCompiler { runSimpleCompilerInternal :: State SimpleCompilerState a }
  deriving (Functor, Applicative, Monad)

emptyCompilerState :: SimpleCompilerState
emptyCompilerState
    = SimpleCompilerState
      V.empty
      emptySymbolTable
      V.empty

simpleCompiler :: Program -> CompilerResult
simpleCompiler prog
    | V.null $ compilerErrors finalState
        = CompileSucceeded
        ( CompiledProgram $ V.toList $ instructions finalState )
        ( symbolTable finalState )
    | otherwise = CompileFailed $ V.toList $ compilerErrors finalState
  where
    finalState = flip execState emptyCompilerState $ do
        runSimpleCompilerInternal $ do
            compile prog
            
    
    
maxKey :: Ord k => Map k a -> Maybe k
maxKey m = case M.keys m of
    [] -> Nothing
    ks -> Just $ maximum ks

maxElem :: (Ord k, Ord a) => Map k a -> Maybe a
maxElem m = case M.elems m of
    [] -> Nothing
    es -> Just $ maximum es

getCurrentAddress :: SimpleCompiler Address
getCurrentAddress 
    = SimpleCompiler 
    $ liftM (Address . mkInteger . V.length) 
    $ gets instructions


nextAvailibleSystemLabel :: SimpleCompiler SystemLabel
nextAvailibleSystemLabel
    = SimpleCompiler 
    $ liftM (maybe 0 succ . maxKey) 
    $ gets (systemLabels . symbolTable)

nextAvailibleVarSymbol :: SimpleCompiler Symbol
nextAvailibleVarSymbol
    = SimpleCompiler 
    $ liftM (maybe 0 succ . maxElem)
    $ gets (varSymbols . symbolTable)

nextAvailibleFuncSymbol :: SimpleCompiler Symbol
nextAvailibleFuncSymbol
    = SimpleCompiler 
    $ liftM (maybe 0 succ . maxElem) 
    $ gets (funcSymbols . symbolTable)

modifyCompilerState :: (SimpleCompilerState -> SimpleCompilerState) -> SimpleCompiler ()
modifyCompilerState = SimpleCompiler . modify

modifySymbolTable :: (SymbolTable -> SymbolTable) -> SimpleCompiler ()
modifySymbolTable f = modifyCompilerState $ \st -> st{ symbolTable = f $ symbolTable st }

modifyInstructions :: (Vector Instruction -> Vector Instruction) -> SimpleCompiler ()
modifyInstructions f = modifyCompilerState $ \st -> st{ instructions = f $ instructions st }

modifyUserLabels :: (Map Snobol4String Address -> Map Snobol4String Address) -> SimpleCompiler ()
modifyUserLabels f = modifySymbolTable $ \st -> st{ userLabels = f $ userLabels st }

modifySystemLabels :: (Map SystemLabel (Maybe Address) 
                   -> Map SystemLabel (Maybe Address)) 
                   -> SimpleCompiler ()
modifySystemLabels f = modifySymbolTable $ \st -> st{ systemLabels = f $ systemLabels st }

modifyVarSymbols :: (Map Snobol4String Symbol -> Map Snobol4String Symbol) -> SimpleCompiler ()
modifyVarSymbols f = modifySymbolTable $ \st -> st{ varSymbols = f $ varSymbols st }

modifyFuncSymbols :: (Map Snobol4String Symbol -> Map Snobol4String Symbol) -> SimpleCompiler ()
modifyFuncSymbols f = modifySymbolTable $ \st -> st{ funcSymbols = f $ funcSymbols st }

modifyCompilerErrors :: (Vector (Address, CompilerError) -> Vector (Address, CompilerError))
                     -> SimpleCompiler ()
modifyCompilerErrors f = modifyCompilerState $ \st -> st{ compilerErrors = f $ compilerErrors st }

instance Compiler SimpleCompiler where
    addUserLabel lbl = do
        addr <- getCurrentAddress
        modifyUserLabels $ M.insert lbl addr
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
                sym <- nextAvailibleVarSymbol
                modifyVarSymbols $ M.insert ident sym
                return sym
            Just sym -> return sym
    getFuncSymbol ident = do
        result <- SimpleCompiler $ liftM (M.lookup ident) $ gets $ (funcSymbols . symbolTable)
        case result of
            Nothing -> do
                sym <- nextAvailibleFuncSymbol
                modifyFuncSymbols $ M.insert ident sym
                return sym
            Just sym -> return sym
    compileError err = do
        addr <- getCurrentAddress
        modifyCompilerErrors $ flip V.snoc (addr, err)
    getPanicLabel = return $ -1
