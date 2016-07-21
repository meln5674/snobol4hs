{-|
Module          : Language.Snobol4.Interpreter.Types
Description     : Types used by the SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Types 
    ( EvalStop (..)
    , Lookup (..)
    , Data (..)
    , Pattern (..)
    , ProgramError (..)
    , Snobol4String
    , Snobol4Integer
    , Snobol4Real
    , Snobol4Read (..)
    , Snobol4IntegerClass (..)
    , Snobol4RealClass (..)
    , Snobol4StringClass (..)
    , snobol4Head
    , snobol4Length
    , snobol4Take
    , snobol4Drop
    , snobol4Elem
    , snobol4NotElem
    , snobol4Replace
    , snobol4Trim
    , nullString
    , (<>)
    ) where

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.Types
