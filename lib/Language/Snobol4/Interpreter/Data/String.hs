{-|
Module          : Language.Snobol4.String.Data.String
Description     : String Datatype
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Snobol4.Interpreter.Data.String 
    ( module Language.Snobol4.Interpreter.Data.String 
    , Snobol4StringClass (..)
    , Snobol4String
    ) where

import qualified Data.Map as M

import Language.Snobol4.Interpreter.Data.Types

-- | Generalization of head
snobol4Head :: Snobol4String -> Maybe Snobol4String
snobol4Head (Snobol4String (c:cs)) = Just $ Snobol4String [c]
snobol4Head _ = Nothing

-- | Split a string into its head and tail
snobol4Uncons :: Snobol4String -> Maybe (Snobol4String,Snobol4String)
snobol4Uncons (Snobol4String (x:xs)) = Just (Snobol4String [x],Snobol4String xs)
snobol4Uncons _ = Nothing

snobol4Last :: Snobol4String -> Maybe Snobol4String
snobol4Last (Snobol4String "") = Nothing
snobol4Last (Snobol4String s) = Just $ Snobol4String [last s]

-- | Generalization of length
snobol4Length :: Snobol4String -> Snobol4Integer
snobol4Length (Snobol4String s) = Snobol4Integer $ length s

snobol4Null :: Snobol4String -> Bool
snobol4Null (Snobol4String "") = True
snobol4Null _ = False

-- | Generalization of take
snobol4Take :: Snobol4Integer -> Snobol4String -> Snobol4String
snobol4Take (Snobol4Integer i) (Snobol4String s) = Snobol4String $ take i s

-- | Generalization of drop
snobol4Drop :: Snobol4Integer -> Snobol4String -> Snobol4String
snobol4Drop (Snobol4Integer i) (Snobol4String s) = Snobol4String $ drop i s

-- | Generalization of elem
snobol4Elem :: Snobol4String -> Snobol4String -> Bool
snobol4Elem (Snobol4String [c]) (Snobol4String s) = c `elem` s
snobol4Elem _ _ = error "Internal error: Invalid call to snobol4Elem"

-- | Generalization of notElem
snobol4NotElem :: Snobol4String -> Snobol4String -> Bool
snobol4NotElem (Snobol4String [c]) (Snobol4String s) = c `notElem` s
snobol4NotElem _ _ = error "Internal error: Invalid call to snobol4NotElem"

-- | Generalization of show
snobol4Show :: Snobol4StringClass a => a -> String
snobol4Show = getString . mkString

-- | Take the third string, and replace each character that appears in the
-- first string with the coresponding character in the second string
snobol4Replace :: Snobol4String -> Snobol4String -> Snobol4String -> Snobol4String
snobol4Replace (Snobol4String xs) (Snobol4String ys) (Snobol4String zs) = Snobol4String $ loop xs
  where
    charMap = M.fromList $ zip ys zs
    loop [] = []
    loop (c:cs) = case M.lookup c charMap of
        Just c' -> c':loop cs
        Nothing -> c:loop cs

-- | Remove whitespace from the end of a string
snobol4Trim :: Snobol4String -> Snobol4String
snobol4Trim (Snobol4String s) = Snobol4String $ loop s
  where
    loop s = case break (==' ') s of
        (a,b) -> case span (==' ') b of
            (c,"") -> a
            (c,d) -> a ++ c ++ loop d


-- | The null string
nullString :: Snobol4String
nullString = Snobol4String ""


-- | Identity
instance Snobol4StringClass Snobol4String where
    mkString = id
    unmkString = id

-- | Convert to/from a string that has the matching internal string
instance Snobol4StringClass String where
    mkString = Snobol4String
    unmkString = getString


    
