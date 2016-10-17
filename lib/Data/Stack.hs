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

newtype Stack a = Stack { getStack :: [a] }

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x,Stack xs)

unsafePop :: Stack a -> (a, Stack a)
unsafePop stk = case pop stk of
    Just (x,stk') -> (x,stk')
    Nothing -> error "unsafePop: empty stack"

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

unsafePeek :: Stack a -> a
unsafePeek stk = case peek stk of
    Just x -> x
    Nothing -> error "unsafePeek: empty stack"

modifyTop :: (a -> a) -> Stack a -> Maybe (Stack a)
modifyTop _ (Stack []) = Nothing
modifyTop f (Stack (x:xs)) = Just $ Stack $ (f x):xs

unsafeModifyTop :: (a -> a) -> Stack a -> Stack a
unsafeModifyTop f stk = case modifyTop f stk of
    Just stk' -> stk'
    Nothing -> error "unsafeModifyTop: empty stack"

get :: Int -> Stack a -> Maybe a
get n (Stack xs) = case drop n xs of
    [] -> Nothing
    (x:_) -> Just x

unsafeGet :: Int -> Stack a -> a
unsafeGet n stk = case get n stk of
    Just x -> x
    Nothing -> error "unsafeGet: out of bounds"

set :: Int -> a -> Stack a -> Maybe (Stack a)
set n x (Stack xs) = case drop n xs of
    [] -> Nothing
    (_:xs') -> Just $ Stack $ (take n xs) ++ x : xs'

unsafeSet :: Int -> a -> Stack a -> Stack a
unsafeSet n x stk = case set n x stk of
    Just stk' -> stk'
    Nothing -> error "unsafeSet: out of bounds"

fromList :: [a] -> Stack a
fromList = Stack

toList :: Stack a -> [a]
toList = getStack
