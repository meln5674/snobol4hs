module Language.Snobol4.Interpreter.Primitives.Prototypes where

import Language.Snobol4.Interpreter.Types

type Dimension = (Snobol4Integer,Snobol4Integer)

type Dimensions = [Dimension]

data ArrayPrototype = ArrayPrototype Dimensions

data FunctionPrototype = FunctionPrototype Snobol4String [Snobol4String] [Snobol4String]
