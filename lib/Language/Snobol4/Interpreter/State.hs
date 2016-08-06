{-|
Module          : Language.Snobol4.Interpreter.State
Description     : State of the interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Functions relating to the state of the interpreter
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Snobol4.Interpreter.State where

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.Types
import Language.Snobol4.Interpreter.Primitives
import Language.Snobol4.Interpreter.Shell

import qualified Data.Map as M

import qualified Data.Vector as V


