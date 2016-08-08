{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.GC
Description     : Garbage collection
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.GC where

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Shell
import Language.Snobol4.Interpreter.Data.Types


import Language.Snobol4.Interpreter.Internal.StateMachine.GC.Types
import Language.Snobol4.Interpreter.Internal.StateMachine.Types

import Language.Snobol4.Interpreter.Data

-- | Mark an object as having no references
newRef :: t -> RefCounted t
newRef x = RefCounted (x,0)

-- | Increment the reference counter of an object
incRefCount :: RefCounted t -> RefCounted t
incRefCount (RefCounted (x,i)) = RefCounted (x,i+1)

-- | Decrement the reference counter of an object
decRefCount :: RefCounted t -> Maybe (RefCounted t)
decRefCount (RefCounted (x,0)) = Nothing
decRefCount (RefCounted (x,i)) = Just $ RefCounted (x,i-1)

-- | Get the item whose references are counted
getRefItem :: RefCounted t -> t
getRefItem (RefCounted (x,_)) = x

-- | Get the number of references to an object
getRefCount :: RefCounted t -> Int
getRefCount (RefCounted (_,i)) = i
