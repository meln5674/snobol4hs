module Language.Snobol4.Interpreter.Data.ObjectCode
    ( module Language.Snobol4.Interpreter.Data.ObjectCode
    , Snobol4Code
    , CodeKey
    ) where

import Language.Snobol4.Syntax.AST
import Language.Snobol4.Interpreter.Data.Types

newCode :: Program -> Snobol4Code
newCode = Snobol4Code
