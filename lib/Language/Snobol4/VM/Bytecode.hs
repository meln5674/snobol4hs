{-|
Module          : Language.Snobol4.VM.Bytecode
Description     : TODO
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

TODO
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.VM.Bytecode where

import Control.Monad

import Data.Serialize
import GHC.Generics

import Data.Map (Map)
import qualified Data.Map as M

import Data.Vector (Vector)
import qualified Data.Vector as V 

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Interpreter.Internal.StateMachine
import Language.Snobol4.Syntax.AST

type ExprKey = SystemLabel

data LabeledInstruction = LabeledInstruction Snobol4String Instruction

newtype CompiledProgram = CompiledProgram { getCompiledProgram :: Vector Instruction }
  deriving Show

newtype Symbol = Symbol { getSymbol :: Snobol4String }
  deriving (Show, Eq, Ord)

newtype SystemLabel = SystemLabel Snobol4Integer
  deriving (Show, Eq, Ord, Enum, Num)


data SymbolTable
    = SymbolTable
    { userLabels :: Map Snobol4String Address
    , systemLabels :: Map SystemLabel (Maybe Address)
    , varSymbols :: Map Snobol4String Symbol
    , funcSymbols :: Map Snobol4String Symbol
    , programEntryPoint :: Address
    }
  deriving Show

emptySymbolTable :: SymbolTable
emptySymbolTable
    = SymbolTable
      M.empty
      M.empty
      M.empty
      M.empty
      (Address 0)


-- | A bytecode instruction.
-- Constructors are documented using the following format:
-- vN : Pop N items from the stack
-- $N : Reference the Nth item popped from the stack (1-based)
-- ^X : Push X onto the stack
-- !N : Reference the Nth argument of the constructor (1-based)
-- X; Y : Do X, then do Y
-- X ? Y : Z : If X, then do Y, else do Z
-- X := Y : Set X to the value of Y
data Instruction
    = 
    -- | Push a value onto the stack
      PushString Snobol4String
    | PushInteger Snobol4Integer
    | PushReal Snobol4Real
    | PushReference Snobol4String
    | PushReferenceKeyword Snobol4String
    | PushReferenceAggregate Snobol4String Snobol4Integer
    | PushReferenceInput
    | PushReferenceOutput
    | PushReferencePunch
    | PushExpression SystemLabel
    -- | Pop the top value off of the stack
    | Pop
    -- | Make N copies of the top value of the stack
    | Copy Int
    -- | v2; ^($1); ^($2)
    | Rotate
    
    -- | v2; ^($2 + $1)
    | Add
    -- | v2; ^($2 - $1)
    | Subtract
    -- | v2; ^($2 * $1)
    | Multiply
    -- | v2; ^($2 / $1)
    | Divide
    -- | v2; ^($2 ** $1)
    | Exponentiate

    -- | v2; ^($2 ++ $1)
    -- Items and result are strings
    | ConcatString
    -- | v2; ^($2 ++ $1)
    -- Items and result are patterns
    | ConcatPattern
    -- | v2; ^($2 | $1)
    | AlternatePattern
    
    -- | v2; ($2 > $1) ? ^1 : ^0
    | GreaterThan
    -- | v2; ($2 >= $1) ? ^1 : ^0
    | GreaterThanOrEqualTo
    -- | v2; ($2 < $1) ? ^1 : ^0
    | LessThan
    -- | v2; ($2 <= $1) ? ^1 : ^0
    | LessThanOrEqualTo
    -- | v2; ($2 == $1) ? ^1 : ^0
    | Equal
    -- | v2; ($2 /= $1) ? ^1 : ^0
    | NotEqual
    
    -- TODO: Rest of the patterns
    -- | v2; ^(AssignmentPattern $2 $1)
    | PrimitiveAssignmentPattern
    -- | v2; ^(ImmediateAssignmentPattern $2 $1)
    | PrimitiveImmediateAssignmentPattern
    -- | v1; ^(LiteralPattern $1)
    | PrimitiveLiteralPattern
    -- etc
    -- | v1; ^(AssignmentPattern $1)
    | PrimitiveAnyPattern
    -- etc
    
    -- | v1; ^(value pointed to by $1)
    | LookupDynamic
    -- | ^(value pointed to by !1)
    | LookupStatic Symbol
    -- | ^(value pointed to by &!1)
    -- | v(!2); ^(!1<$1,$2,$...,$(!2)>    
    | LookupStaticRef Symbol Int
    | LookupStaticKeyword Symbol
    
    -- | v1; !1 := $1
    | AssignStatic Symbol
    -- | v1; &!1 := $1
    | AssignStaticKeyword Symbol
    -- | v(!2 + 1); !1<$1, $2, ... $(!2)> := $(!2 + 1)
    | AssignRefStatic Symbol Int
    -- | v2; $1 := $2
    | AssignDynamic

    -- | v2; Create a new function using $1 as the entry label and $2 as the prototype
    | Define
    -- | ???
    | CallDynamic
    -- | 
    | CallStatic Symbol Int Bool
    -- | ^(arg count)
    | GetArgCount
    -- | v(2 + local counter + arg counter);
    --   Jump to $1;
    --   ^($3)
    | Return
    -- | v(2 + local counter + arg counter);
    --   Jump to $1;
    --   ^failure
    | FReturn
    
    -- | v(2 + local counter + arg counter);
    --   Jump to $1;
    --   ^($3 as a reference)
    | NReturn

    -- | Return from an unevaluated expression
    | ExprReturn

    
    -- | Set the fail label to !1
    | SetFailLabel SystemLabel
    -- | ^(fail label)
    | PushFailLabel SystemLabel
    -- | v1;
    -- fail label = $1
    | PopFailLabel
    -- | Jump to the failure label
    | JumpToFailureLabel
    -- | v1; $1 == 0 || $1 == "" ? Jump to the failure label : noop
    | JumpToFailureLabelIf
    -- | v1; $1 == 0 || $1 == "" ? noop : Jump to the failure label
    | JumpToFailureLabelElse

    -- | Jump to !1
    | JumpStatic SystemLabel
    -- | v1; $1 == 0 || $1 == "" ? Jump to !1 : noop
    | JumpStaticIf SystemLabel
    -- | v1; $1 == 0 || $1 == "" ? noop : Jump to !1
    | JumpStaticElse SystemLabel
    -- | v1; Jump to $1
    | JumpDynamic
    -- | v1; ^(current address); Direct jump to $1
    | DirectJump
    
    -- | Look up function for !1 and call its function
    | BinOp Operator
    -- | Lookup up function for !1 and call its function
    | UnOp Operator
    
    -- | v2; Invoke the scanner, using $1 as the pattern value and $2 as the
    -- value to scan; ^(end index); ^(start index)
    | InvokeScanner
    -- | v4; Invoke the replacer, replacing the segment between indices $2
    -- and $3 in $4 with $1; ^(new value)
    | InvokeReplacer
    
    -- | v1; $1 is stringable ? ^(string $1) : ^failure
    | ConvertToString
    -- | v1; $1 is integerable ? ^(integer $1) : ^failure
    | ConvertToInteger
    -- | v1; $1 is realable ? ^(real $1) : ^failure
    | ConvertToReal
    -- | v1; $1 is patternable ? ^(pattern $1) : ^failure
    | ConvertToPattern
    
    -- | v2; Invoke the array allocator using $1 as the initial value, and $2 as
    -- the prototype
    | AllocArray
    -- | v1; Invoke the table allocator using $1 as the initial size
    | AllocTable

    -- | Panic with the given error
    | Panic ProgramError
    -- | Terminate the program successfully
    | Finish

    -- | v1; write $1 to the terminal
    | Output
    -- | read X from the terminal; ^X
    | Input
    | Punch
  
    | LastOutput
    | LastPunch
  deriving (Show)

deriving instance Serialize Address
deriving instance Generic SymbolTable
deriving instance Generic Operator
deriving instance Generic ProgramError
deriving instance Generic SystemLabel
deriving instance Generic Symbol
deriving instance Generic Instruction

instance Serialize a => Serialize (Vector a) where
    get = liftM V.fromList get
    put = put . V.toList

instance Serialize Snobol4String where
    put = put . (unmkString :: Snobol4String -> String)
    get = liftM mkString (get :: Get String)

instance Serialize Snobol4Integer where
    put = put . (unmkInteger :: Snobol4Integer -> Int)
    get = liftM mkInteger (get :: Get Int)

instance Serialize Snobol4Real where
    put = put . (unmkReal :: Snobol4Real -> Float)
    get = liftM mkReal (get :: Get Float)

instance Serialize Operator
instance Serialize ProgramError
instance Serialize SystemLabel
instance Serialize Symbol
instance Serialize Instruction
instance Serialize SymbolTable

deriving instance Serialize CompiledProgram

