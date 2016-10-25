{-|
Module          : Language.Snobol4.Interpreter.Shell.Mock
Description     : Mock implementation of the InterpreterShell typeclass
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

This implementation of InterpreterShell allows the user to specify inputs and
start time in advance, then inspect the values later.

(actionResult,shellResult) <- getMockResults $ shell $ do
    start
    -- do stuff with the interpreter
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Language.Snobol4.Interpreter.Shell.Mock
    ( MockShellT
    , MockResultsT (..)
    , MockShellResults (..)
    , mkMockShell
    , addInput
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Functor.Identity

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Language.Snobol4.Interpreter.Shell

-- | Transformer for running a mock shell
newtype MockShellT m a = MockShellT { runMockShellTInternal :: StateT MockShellState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Transformer for retreiving mock results
newtype MockResultsT m a = MockResultsT { getMockResultsT :: m (a,MockShellResults)  }

        

-- | Monad for running a mock shell
type MockShell = MockShellT Identity

-- | Monad for retreiving mock results
type MockResults = MockResultsT Identity

-- | Retreive the results of a mock shell
getMockResults :: MockResults a -> (a,MockShellResults)
getMockResults = runIdentity . getMockResultsT

-- | Results of a mock shell
data MockShellResults
    = MockShellResults
    { 
    -- | Inputs that have not yet been consumed
      inputs :: Vector String
    -- | Ouputs
    , outputs :: Vector String
    -- | Punches
    , punches :: Vector String
    -- | The time last set
    , mockTime :: Int
    -- | The date last set
    , mockDate :: String
    }

-- | Synonym for the results
type MockShellState = MockShellResults

-- | Initial state of the mock shell
initialMockShellState = MockShellResults
    V.empty
    V.empty
    V.empty
    0
    ""
-- | Add an input to be read
addInput :: Monad m => String -> MockShellT m ()
addInput s = MockShellT $ modify $ \st -> st{ inputs = V.snoc (inputs st) s }

-- | Set the date
setDate :: Monad m => String -> MockShellT m ()
setDate d = MockShellT $ modify $ \st -> st{ mockDate = d }

-- | Set the time
setTime :: Monad m => Int -> MockShellT m ()
setTime t = MockShellT $ modify $ \st -> st{ mockTime = t }

-- | Use mocked out values for input and output
instance Monad m => InterpreterShell (MockShellT m) where
    input = MockShellT $ state $ \st -> flip ($) (inputs st) $ \xs -> 
        if V.null xs 
            then (Nothing, st)
            else (Just $ V.head xs, st{ inputs = V.tail xs })
    output x = MockShellT $ modify $ \st -> st{ outputs = V.snoc (outputs st) x }
    punch x = MockShellT $ modify $ \st -> st{ punches = V.snoc (punches st) x }
    lastOutput = MockShellT $ gets $ flip (.) outputs $ \xs ->
        if V.null xs
            then ""
            else V.last xs
    lastPunch = MockShellT $ gets $ flip (.) punches $ \xs ->
        if V.null xs
            then ""
            else V.last xs
    date = MockShellT $ gets mockDate
    time = MockShellT $ gets mockTime

-- | Run the shell with mocked out inputs and outputs and collect them
instance Monad m => InterpreterShellRun (MockShellT m) where
    type BaseMonad (MockShellT m) = MockResultsT m
    start = return ()
    shell f = MockResultsT $ runStateT (runMockShellTInternal f) initialMockShellState

-- | Don't ask
--
-- See Language.Snobol4.VM.Bytecode.Interpreter.Internal.mkVM
mkMockShell :: ( forall s
                  .  ( forall b . MockShellT m b -> s -> m (b, s) )
                  -> ( forall b . (s -> m (b, s)) -> MockShellT m b )
                  -> ( s -> m (a, s) )
                  ) 
               -> MockShellT m a
mkMockShell f = MockShellT $ StateT $ f runFunc stateFunc
  where
    runFunc g st = runStateT (runMockShellTInternal g) st
    stateFunc = MockShellT . StateT

