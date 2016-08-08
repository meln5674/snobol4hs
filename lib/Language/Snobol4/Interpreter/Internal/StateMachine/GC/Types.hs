{-|
Module          : Language.Snobol4.Internal.StateMachine.GC.Types
Description     : Types used by the state machine's garbage collector
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}
module Language.Snobol4.Interpreter.Internal.StateMachine.GC.Types where

-- | A pair of an item and the number of references to it
newtype RefCounted t = RefCounted (t, Int) deriving Show

-- | 
instance Functor RefCounted where
    fmap f (RefCounted (x,i)) = RefCounted (f x,i)
