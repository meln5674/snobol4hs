{-|
Module          : Language.Snobol4.Interpreter.Data.Lazy
Description     : Lazy data types
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


-}

module Language.Snobol4.Interpreter.Data.Lazy
    ( LazyPattern
    , LazyString
    , LazyInteger
    , Lazy(..)
    , orLazy
    ) where

import Language.Snobol4.Interpreter.Data.Types
