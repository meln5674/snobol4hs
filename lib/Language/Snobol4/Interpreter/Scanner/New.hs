{-|
Module          : Language.Snobol4.Interpreter.Scanner.New
Description     : The SNOBOL4 Interpreter
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

Temporary home for the quickscan experiment.

The functions in this module make up the experimental quickscan scanner. This
    scanner uses hueristics to avoid attempting matches which are impossible.
    This also results in eliminating some cases of left-recursion.

Quickscanning is performed in the following steps:

1) Convert a pattern to a "Fullscan path", which separates out pattern
    combinators (such as |, ARBNO, etc) from leaves (such as literals, BREAK,
    etc). This allows the scanner to have more context about alternatives.
    Unevaluated patterns and parameters are left unevaulated.

2) Convert the Fullscan path to an incomplete quickscan path. A incomplete
    quickscan path is the same as a fullscan path, but each node is labled with
    an optional (initially absent) count of how many characters, at minimum, that
    node will require to be matched.

3) Traverse the incomplete quickscan path and compute the minimum characters for
    each node. 

4) Clean up the quickscan tree by converting the optional counters to mandatory
    ones, forming a complete quickscan tree.

5) Traverse the completed tree and search for unevaluated patterns. If one is
    found, evaluate it, and perform steps 1-4 to transform it into a quickscan
    path. If that path requires more characters than is allowed, replace the
    unevaluated pattern with a dead node which will always cause failure. If
    the path has an acceptable length, repeat this process to fully expand it.
    Once the path has been fully expanded, replace the unevaluated pattern with
    the new path.

6) The path is now ready for matching. Use a continuation passing scheme to
    descend into pattern, backtracking as necessary.
-}


{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.Interpreter.Scanner.New where

import Prelude hiding (repeat)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Language.Snobol4.Interpreter.Data

import Language.Snobol4.Interpreter.Scanner.New.QuickScan
import Language.Snobol4.Interpreter.Scanner.New.FullScan

runScan :: (Monad m)
          => Pattern expr
          -> Snobol4String
          -> Bool
          -> Bool
          -> (expr -> m (Maybe (Pattern expr)))
          -> (expr -> m (Maybe Snobol4Integer))
          -> (expr -> m (Maybe Snobol4String))
          -> (Lookup expr -> Data expr -> m ())
          -> m (Maybe Snobol4String)
runScan pat toMatch anchorMode False toPat toInt toStr set =
    quickscan pat toMatch anchorMode toPat toInt toStr set
runScan pat toMatch anchorMode True toPat toInt toStr set =
    fullscan pat toMatch anchorMode toPat toInt toStr set
