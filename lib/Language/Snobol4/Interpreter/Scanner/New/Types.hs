{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Language.Snobol4.Interpreter.Scanner.New.Types 
    ( module Language.Snobol4.Interpreter.Scanner.New.Types 
    , ScanResult (..)
    ) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Internal.StateMachine.Types (ScanResult(..))

-- | A reason for scanner failure
data ScanFailure
    = 
    -- | The scanner encountered a dead end, and needs to try an alternative
    -- path
      Backtrack 
    -- | The scanner encountered an error it could not recover from
    | Abort 
    -- | The scanner ecnountered a dead end by finding a node that required too
    -- many characters
    | NotEnoughCharacters

-- | A "leaf" pattern, one that does not reference any other patterns and simply
-- matches a string
data ScanPattern expr
    = 
    -- | Matches the given string exactly
      ScanLiteral Snobol4String
    -- | Matches any one character in the given string
    | ScanAny (LazyString expr)
    -- | Matches any one character not in the given string
    | ScanNotAny (LazyString expr)
    -- | Matches the longest string containing only characters in the given string
    | ScanSpan (LazyString expr)
    -- | Matches the longest string containing none of the characters in the given string
    | ScanBreak (LazyString expr)
    -- | Matches the given number of characters
    | ScanLen (LazyInteger expr)
    -- | Matches rest of input
    | ScanRemainder
    -- | Matches null string, assigns cursor position to variable
    | ScanHead (Lookup expr)
    | ScanTab (LazyInteger expr)
    | ScanRTab (LazyInteger expr)
    | ScanPos (LazyInteger expr)
    | ScanRPos (LazyInteger expr)
    | ScanFail
    | ScanFence
    | ScanAbort
    | ScanArb
    | ScanBal
    | ScanSucceed
  deriving Show

-- | A "branch" battern, one that is made up of one or more other patterns
-- along with an annotation
data ScanPath expr ann
    = 
    -- | Matches the first pattern, then the second
      ScanConcat (ScanPath expr ann) (ScanPath expr ann) ann
    -- | Matches the first pattern, if it fails, it matches the second
    | ScanChoice (ScanPath expr ann) (ScanPath expr ann) ann
    -- | Matches the given pattern, then marks the matched string to be assigned
    -- to the given variable once scanning is complete
    | ScanAssign (ScanPath expr ann) (Lookup expr) ann
    -- Matches the given pattern, then immediately assigns the matched string to
    -- the given variable
    | ScanImmediateAssign (ScanPath expr ann) (Lookup expr) ann
    -- | Matches the given pattern any number of times
    | ScanArbNo (ScanPath expr ann) ann
    -- | Matches the given leaf pattern
    | ScanNode (ScanPattern expr) ann
    -- | An pattern which has not yet been evaluated
    | UnevaluatedPattern expr
    -- | A dead node, if encountered, the scanner should fail
    | DeadNode
  deriving Show

-- | A fullscan path, nodes have no annotation
type FullScanPath expr = ScanPath expr ()


data ScannerState expr = ScannerState
    { toMatch :: Snobol4String
    , matched :: Snobol4String
    , prevMatch :: Snobol4String
    , assignments :: [(Lookup expr, Data expr)]
    , offset :: Snobol4Integer
    }

{-
data ScannerEnv expr m = ScannerEnv
    { anchorMode :: Bool
    , fullscanMode :: Bool
    , resolvePatternFunc :: expr -> m (Maybe (Pattern expr))
    , resolveIntegerFunc :: expr -> m (Maybe Snobol4Integer)
    , resolveStringFunc :: expr -> m (Maybe Snobol4String)
    , assignFunc :: Lookup expr -> Data expr -> m ()
    }


newtype ScannerEnvT expr m a = ScannerEnvT
    { runScannerEnvT :: ReaderT (ScannerEnv expr m) m a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance MonadTrans (ScannerEnvT expr) where
    lift = ScannerEnvT . lift
-}

newtype ScannerT expr m a = ScannerT
    { runScannerT :: ExceptT ScanFailure 
                    (StateT (ScannerState expr)
--                    (ScannerEnvT expr
                     m){-)-} a 
    }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ScannerT expr) where
    lift = ScannerT . lift . lift {-. lift-}

-- | Continuation monad for the quickscanner
type ScannerContT expr m a = ScannerT expr m a -> ScannerT expr m a


class Monad m => ResolveClass' m where
    type Resolvable m
    resolvePattern :: Resolvable m -> m (Maybe (Pattern (Resolvable m)))
    resolveInteger :: Resolvable m -> m (Maybe Snobol4Integer)
    resolveString :: Resolvable m -> m (Maybe Snobol4String)
    immediateAssign :: Lookup (Resolvable m) -> Data (Resolvable m) -> m ()

{-
instance Monad m => ResolveClass (ScannerEnvT expr m) where
    type Resolvable (ScannerEnvT expr m) = expr
    resolvePattern expr = (ScannerEnvT $ asks resolvePatternFunc) >>= lift . ($ expr)
    resolveInteger expr = (ScannerEnvT $ asks resolveIntegerFunc) >>= lift . ($ expr)
    resolveString expr = (ScannerEnvT $ asks resolveStringFunc) >>= lift . ($ expr)
    assign l x = (ScannerEnvT $ asks assignFunc) >>= lift . ($ (l,x)) . uncurry
-}

instance ( Monad m 
         , ResolveClass expr m
         )
      => ResolveClass' (ScannerT expr m) where
    type Resolvable (ScannerT expr m) = expr
    resolvePattern = ScannerT . lift . lift . resolvePattern
    resolveInteger = ScannerT . lift . lift . resolveInteger
    resolveString = ScannerT . lift . lift . resolveString
    immediateAssign l = ScannerT . lift . lift . immediateAssign l

type ResolveClass expr m = (ResolveClass' m, Resolvable m ~ expr)

class RequiredCharClass a where
    isSufficient :: Snobol4Integer -> a -> Bool

type family ScannerPath (full :: Bool) expr
