{-|
Module          : Language.Snobol4.Interpreter.Internal.StateMachine.Functions
Description     : Maintaining and calling functions
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.Interpreter.Internal.StateMachine.Functions 
    ( noFunctions
    , getFunctions
    , putFunctions
    , modifyFunctions
    , clearFunc
    , funcLookup
    , functionsNew
    , selectorFunctionsNew
    , constructorFunctionsNew
    
    , noUnOpSyns
    , noBinOpSyns
    , lookupUnOpSyn
    , lookupBinOpSyn
    , putUnOpSyns
    , putBinOpSyns
    , setUnOpSyn
    , setBinOpSyn
    ) where

import Language.Snobol4.Interpreter.Internal.StateMachine.FuncOps
