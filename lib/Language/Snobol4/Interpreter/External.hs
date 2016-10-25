{-|
Module          : Language.Snobol4.Interpreter.External
Description     : External Functions
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

External functions and data are handled by the Extern type. Types which
can be converted to and from Extern are memeberes of the ExternClass and
UnExternClass classes respectively.

External functions can be called using callExtern and providing a list of
arguments, converted to Extern.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Snobol4.Interpreter.External 
    ( Extern
    , ExternClass (..)
    , callExtern
    ) where

import Control.Monad

import Language.Snobol4.Interpreter.Data

-- | An external value
data Extern
    = ExternInteger Snobol4Integer
    | ExternReal Snobol4Real
    | ExternString Snobol4String
    | ExternIntegerFunction (Snobol4Integer -> Extern)
    | ExternRealFunction (Snobol4Real -> Extern)
    | ExternStringFunction (Snobol4String -> Extern)

-- | Types which can be converted to an Extern
class ExternClass a where
    -- | Convert to an Extern
    extern :: a -> Extern

-- | Types which can be converted from an Extern
class ExternClass a => UnExternClass a where
    -- | Convert from an Extern
    unExtern :: Extern -> Maybe a

instance ExternClass Snobol4Integer where
    extern = ExternInteger

instance UnExternClass Snobol4Integer where
    unExtern (ExternInteger i) = Just i
    unExtern _ = Nothing

instance ExternClass Int where
    extern = ExternInteger . mkInteger

instance UnExternClass Int where
    unExtern = liftM unmkInteger . unExtern

instance ExternClass Snobol4Real where
    extern = ExternReal

instance UnExternClass Snobol4Real where
    unExtern (ExternReal r) = Just r
    unExtern _ = Nothing

instance ExternClass Float where
    extern = ExternReal . mkReal

instance UnExternClass Float where
    unExtern = liftM unmkReal . unExtern

instance ExternClass Snobol4String where
    extern = ExternString

instance UnExternClass Snobol4String where
    unExtern (ExternString s) = Just s
    unExtern _ = Nothing

instance ExternClass String where
    extern = ExternString . mkString

instance UnExternClass String where
    unExtern = liftM unmkString . unExtern

instance ExternClass a => ExternClass  (Snobol4Integer -> a) where
    extern = ExternIntegerFunction . (extern .)

instance ExternClass a => ExternClass  (Int -> a) where
    extern f = extern $ f . unmkInteger

instance ExternClass a => ExternClass  (Snobol4Real -> a) where
    extern = ExternRealFunction . (extern .)

instance ExternClass a => ExternClass  (Float -> a) where
    extern f = extern $ f . unmkReal

instance ExternClass a => ExternClass  (Snobol4String -> a) where
    extern = ExternStringFunction . (extern .)

instance ExternClass a => ExternClass  (String -> a) where
    extern f = extern $ f . unmkString

instance ExternClass Char where
    extern = extern . (:[])

instance UnExternClass Char where
    unExtern = liftM head . unExtern

instance ExternClass Extern where
    extern = id

instance UnExternClass Extern where
    unExtern = Just

-- | Call an external function
callExtern :: Extern -> [Extern] -> Maybe Extern
callExtern (ExternIntegerFunction f) (arg:args) = case unExtern arg of
    Just i -> callExtern (f i) args
    Nothing -> Nothing
callExtern (ExternRealFunction f) (arg:args) = case unExtern arg of
    Just r -> callExtern (f r) args
    Nothing -> Nothing
callExtern (ExternStringFunction f) (arg:args) = case unExtern arg of
    Just s -> callExtern (f s) args
    Nothing -> Nothing
callExtern (ExternIntegerFunction f) [] = callExtern (f 0) []
callExtern (ExternRealFunction f) [] = callExtern (f 0.0) []
callExtern (ExternStringFunction f) [] = callExtern (f $ mkString "") []
callExtern v _ = Just v
