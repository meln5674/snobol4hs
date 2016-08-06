module Language.Snobol4.Interpreter.Internal.StateMachine.GC.Types where

newtype RefCounted t = RefCounted (t, Int)

instance Functor RefCounted where
    fmap f (RefCounted (x,i)) = RefCounted (f x,i)
