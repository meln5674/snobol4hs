module Language.Snobol4.Interpreter.Internal.StateMachine.GC where

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data.Types


import Language.Snobol4.Interpreter.Internal.StateMachine.GC.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
{-import Language.Snobol4.Interpreter.Internal.StateMachine.Arrays
import Language.Snobol4.Interpreter.Internal.StateMachine.Tables
import Language.Snobol4.Interpreter.Internal.StateMachine.Patterns
import Language.Snobol4.Interpreter.Internal.StateMachine.ObjectCode-}

import Language.Snobol4.Interpreter.Data

newRef :: t -> RefCounted t
newRef x = RefCounted (x,0)

incRefCount :: RefCounted t -> RefCounted t
incRefCount (RefCounted (x,i)) = RefCounted (x,i+1)

decRefCount :: RefCounted t -> Maybe (RefCounted t)
decRefCount (RefCounted (x,0)) = Nothing
decRefCount (RefCounted (x,i)) = Just $ RefCounted (x,i-1)

getRefItem :: RefCounted t -> t
getRefItem (RefCounted (x,_)) = x

getRefCount :: RefCounted t -> Int
getRefCount (RefCounted (_,i)) = i
