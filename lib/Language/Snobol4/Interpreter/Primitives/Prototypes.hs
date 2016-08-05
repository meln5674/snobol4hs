{-|
Module          : Language.Snobol4.Interpreter
Description     : SNOBOL4 Prototypes
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


-}

module Language.Snobol4.Interpreter.Primitives.Prototypes where

import Language.Snobol4.Interpreter.Data

-- | Dimenion of an 1D array
type Dimension = (Snobol4Integer,Snobol4Integer)

-- | Dimensions of an multidimension array
type Dimensions = [Dimension]

-- | Prototype of an array
data ArrayPrototype = ArrayPrototype Dimensions

-- | Prototype of a function
data FunctionPrototype
    = FunctionPrototype
        Snobol4String
        [Snobol4String]
        [Snobol4String]

data DataPrototype
    = DataPrototype
        Snobol4String
        [Snobol4String]
