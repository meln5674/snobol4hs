{-|
Module          : Data.Stack
Description     : Stack Datatype
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}



module Data.Stack 
    ( Stack
    , empty
    , push
    , pop
    , unsafePop
    , peek
    , unsafePeek
    , modifyTop
    , unsafeModifyTop
    , get
    , unsafeGet
    , set
    , unsafeSet
    , fromList
    , toList
    ) where

-- | A LIFO stack
newtype Stack a = Stack { getStack :: [a] }

-- | Empty stack
empty :: Stack a
empty = Stack []

-- | Push a value onto a stack
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

-- | Pop a value from a stack
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x,Stack xs)

-- | Pop a value from a stack, throw an error if empt
unsafePop :: Stack a -> (a, Stack a)
unsafePop stk = case pop stk of
    Just (x,stk') -> (x,stk')
    Nothing -> error "unsafePop: empty stack"

-- | Look at the top item of a stack
peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

-- | Look at the top item of a stack, throw an error if empty
unsafePeek :: Stack a -> a
unsafePeek stk = case peek stk of
    Just x -> x
    Nothing -> error "unsafePeek: empty stack"

-- | Apply a function to the top of a stack
modifyTop :: (a -> a) -> Stack a -> Maybe (Stack a)
modifyTop _ (Stack []) = Nothing
modifyTop f (Stack (x:xs)) = Just $ Stack $ (f x):xs

-- | Apply a function to the top of a stack, throw an error if empty
unsafeModifyTop :: (a -> a) -> Stack a -> Stack a
unsafeModifyTop f stk = case modifyTop f stk of
    Just stk' -> stk'
    Nothing -> error "unsafeModifyTop: empty stack"

-- | Get the nth item in a stack
get :: Int -> Stack a -> Maybe a
get n (Stack xs) = case drop n xs of
    [] -> Nothing
    (x:_) -> Just x

-- | Get the nth item in a stack, throw an error if insufficient items present
unsafeGet :: Int -> Stack a -> a
unsafeGet n stk = case get n stk of
    Just x -> x
    Nothing -> error "unsafeGet: out of bounds"

-- | Overwrite the nth item in a stack
set :: Int -> a -> Stack a -> Maybe (Stack a)
set n x (Stack xs) = case drop n xs of
    [] -> Nothing
    (_:xs') -> Just $ Stack $ (take n xs) ++ x : xs'

-- | Overwrite the nth item in a stack, throw an error if insufficient items present
unsafeSet :: Int -> a -> Stack a -> Stack a
unsafeSet n x stk = case set n x stk of
    Just stk' -> stk'
    Nothing -> error "unsafeSet: out of bounds"

-- | Convert a list to a stack
fromList :: [a] -> Stack a
fromList = Stack

-- | Convert a stack to a list
toList :: Stack a -> [a]
toList = getStack
