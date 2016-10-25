{-|
Module          : Language.Snobol4.VM
Description     : Bytecode Virtual Machine
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

-}

module Language.Snobol4.VM 
    ( module Language.Snobol4.VM.Bytecode.Interpreter
    , module Language.Snobol4.VM.Bytecode
    ) where

import Language.Snobol4.VM.Bytecode.Interpreter
import Language.Snobol4.VM.Bytecode
