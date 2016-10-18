{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Snobol4.Interpreter.External 
    ( Extern
    , ExternClass (..)
    , callExtern
    ) where

import Control.Monad

import Language.Snobol4.Interpreter.Data

data Extern
    = ExternInteger Snobol4Integer
    | ExternReal Snobol4Real
    | ExternString Snobol4String
    | ExternIntegerFunction (Snobol4Integer -> Extern)
    | ExternRealFunction (Snobol4Real -> Extern)
    | ExternStringFunction (Snobol4String -> Extern)
    
class ExternClass a where
    extern :: a -> Extern

class ExternClass a => UnExternClass a where
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

toExternInteger :: Extern -> Maybe Snobol4Integer
toExternInteger (ExternInteger i) = Just i
toExternInteger _ = Nothing

toExternReal :: Extern -> Maybe Snobol4Real
toExternReal (ExternReal r) = Just r
toExternReal _ = Nothing

toExternString :: Extern -> Maybe Snobol4String
toExternString (ExternString s) = Just s
toExternString _ = Nothing

callExtern :: Extern -> [Extern] -> Maybe Extern
callExtern (ExternIntegerFunction f) (arg:args) = case toExternInteger arg of
    Just i -> callExtern (f i) args
    Nothing -> Nothing
callExtern (ExternRealFunction f) (arg:args) = case toExternReal arg of
    Just r -> callExtern (f r) args
    Nothing -> Nothing
callExtern (ExternStringFunction f) (arg:args) = case toExternString arg of
    Just s -> callExtern (f s) args
    Nothing -> Nothing
callExtern (ExternIntegerFunction f) [] = callExtern (f 0) []
callExtern (ExternRealFunction f) [] = callExtern (f 0.0) []
callExtern (ExternStringFunction f) [] = callExtern (f $ mkString "") []
callExtern v _ = Just v
