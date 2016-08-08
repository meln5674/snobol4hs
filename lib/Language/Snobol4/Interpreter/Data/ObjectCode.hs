{-|
Module          : Language.Snobol4.Interpreter.Data.ObjectCode
Description     : Dynamically compiled object code type
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Data.ObjectCode
    ( module Language.Snobol4.Interpreter.Data.ObjectCode
    , Snobol4Code
    , CodeKey
    ) where

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Data.Types

-- | Create a new object code value from a parsed program
newCode :: Program -> Snobol4Code
newCode = Snobol4Code
