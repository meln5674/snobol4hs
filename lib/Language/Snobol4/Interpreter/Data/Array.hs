{-|
Module          : Language.Snobol4.Interpreter.Data.Array
Description     : Array Datatype
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Data.Array 
    ( module Language.Snobol4.Interpreter.Data.Array 
    , Snobol4Array
    , ArrayKey
    ) where

import Data.Array (Array)
import qualified Data.Array as A

import Language.Snobol4.Interpreter.Data.Types

-- | Utility function, safe lookup for arrays
arrayGet :: A.Ix i => Array i e -> i -> Maybe e
arr `arrayGet` ix
    | inBounds = Just $ arr A.! ix
    | otherwise = Nothing
  where
    inBounds = minB <= ix && ix < maxB
    (minB,maxB) = A.bounds arr 

-- | Create a new array with upper and lower bounds with an initial value for each index
newArray :: Snobol4Integer -> Snobol4Integer -> Data -> Snobol4Array
newArray minIx maxIx v
    = Snobol4Array 
    $ A.array (minIx,maxIx) 
    $ map (\x -> (x,v)) [minIx..maxIx]

-- | Create a new array from a list of pairs of indices and values, the indices
-- are assumed to be contiguous
newArray' :: [(Snobol4Integer,Data)] -> Snobol4Array
newArray' xs = Snobol4Array $ A.array (minIx,maxIx) xs
  where
    minIx = fst $ head xs
    maxIx = fst $ last xs

-- | Get the value of an array at an index
readArray :: Snobol4Integer -> Snobol4Array -> Maybe Data
readArray ix (Snobol4Array arr)
    | minIx <= ix && ix <= maxIx = Just $ arr A.! ix
    | otherwise = Nothing
  where
    (minIx,maxIx) = A.bounds arr

-- | Se the value of an array at an index
writeArray :: Snobol4Integer -> Data -> Snobol4Array -> Snobol4Array
writeArray ix v (Snobol4Array arr) = Snobol4Array $ arr A.// [(ix,v)]

-- | Get the formal identification of an array
-- TODO
arrayFormalIdent :: Snobol4Array -> Snobol4String
arrayFormalIdent arr = undefined
