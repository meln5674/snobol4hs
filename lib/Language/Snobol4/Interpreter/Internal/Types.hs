{-|
Module          : Language.Snobol4.Interpreter
Description     : Low level types and operations
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown


-}

module Language.Snobol4.Interpreter.Internal.Types 
    ( Interpreter
    , Evaluator
    , ProgramError (..)
    ) where

import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Internal.StateMachine.Types
