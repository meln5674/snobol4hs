{-|
Module          : Language.Snobol4.VM.Bytecode
Description     : TODO
Copyright       : (c) Andrew Melnick 2016
License         : MIT
Maintainer      : meln5674@kettering.edu
Portability     : Unknown

TODO
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Snobol4.VM.Bytecode where

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error
import Language.Snobol4.Syntax.AST

data LabeledInstruction = LabeledInstruction Snobol4String Instruction

newtype CompiledProgram = CompiledProgram { getCompiledProgram :: [Instruction] }
  deriving Show

newtype Symbol = Symbol Snobol4Integer
  deriving (Show, Eq, Ord, Enum, Num)

newtype SystemLabel = SystemLabel Snobol4Integer
  deriving (Show, Eq, Ord, Enum, Num)

data Instruction
    = Push Data
    | PushSystemLabel SystemLabel
    | Pop
    | Copy Int
    
    | Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiate
    | And
    | Or
    | Not
    | ConcatString
    | ConcatPattern
    | AlternatePattern
    
    | GreaterThan
    | GreaterThanOrEqualTo
    | LessThan
    | LessThanOrEqualTo
    | Equal
    | NotEqual
    
    | PrimitiveAssignmentPattern
    | PrimitiveImmediateAssignmentPattern
    | PrimitiveLiteralPattern
    -- etc
    | PrimitiveAnyPattern
    -- etc
    
    | LookupDynamic
    | LookupStatic Symbol
    
    | AssignStatic Symbol
    | AssignRefStatic Symbol Int
    | AssignDynamic
    | AssignRefDynamic Int
    
    | CallDynamic
    | CallStatic Symbol Int
    | GetArgCount
    | Return
    | FReturn
    
    | RefDynamic 
    | RefStatic Symbol Int
    
    | SetFailLabel SystemLabel
    | JumpToFailureLabel
    | JumpToFailureLabelIf
    | JumpToFailureLabelElse

    | JumpStatic SystemLabel
    | JumpStaticIf SystemLabel
    | JumpStaticElse SystemLabel
    | JumpDynamic
    | DirectJump
    
    | BinOp Operator
    | UnOp Operator
    
    | InvokeScanner
    | InvokeReplacer
    
    | ConvertToString
    | ConvertToInteger
    | ConvertToReal
    | ConvertToPattern
    
    | AllocArray
    | AllocTable

    | SetError ProgramError
    
    | Panic
    
  deriving (Show)
