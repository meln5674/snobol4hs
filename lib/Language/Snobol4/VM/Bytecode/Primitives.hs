{-# LANGUAGE LambdaCase #-}
module Language.Snobol4.VM.Bytecode.Primitives where

import Prelude hiding (any)

import Data.Monoid

import Data.Map (Map, (!))
import qualified Data.Map as M

import Control.Monad

import Language.Snobol4.Interpreter.Data
import Language.Snobol4.Interpreter.Error

import Language.Snobol4.VM.Bytecode
import Language.Snobol4.VM.Bytecode.Compiler

data PrimitiveOp
    = I Instruction
    | I' (SystemLabel -> Instruction) String
    | L String Instruction

data Primitive = Primitive String [PrimitiveOp]
{-
data PrimOp = PrimOp Operator [PrimitiveOp]
-}
allPrimitives :: [Primitive]
allPrimitives =
    [ any
    , apply
    , array
    ]
{-
allPrimOps :: [PrimiOp]
allPrimOps =
    [ primOp_plus
    ]
-}
addPrimitiveCode :: Compiler m => m ()
addPrimitiveCode = mapM_ mkPrimitive allPrimitives
{-
addPrimOpCode :: Compiler m => m ()
addPrimOpCode = mapM_ 
-}
mkPrimitive :: Compiler m => Primitive -> m ()
mkPrimitive (Primitive _ ps) = do
    labelMap <- mkLabelMap
    forM_ ps $ \case
        I inst -> addInstruction inst
        I' inst lbl -> addInstruction $ inst $ labelMap ! lbl
        L lbl inst -> do
            addSystemLabel (labelMap ! lbl)
            addInstruction inst
  where
    labels = maybe [] id $ foldl (<>) mempty $ flip map ps $ \case
        I _ -> Nothing
        I' _ _ -> Nothing
        L l _ -> Just [l]
    mkLabelMap = foldM registerLabel M.empty labels
    registerLabel m l = do
        sym <- allocSystemLabel
        return $ M.insert l sym m
{-
primOp_plus :: PrimOp
primOp_plus = PrimOp Plus
    [ I $ Add ]
-}
any :: Primitive
any = Primitive "any"
    [ I $ GetArgCount
    , I $ PushInteger 1
    , I $ Equal
    , I $ JumpToFailureLabelElse
    , I $ ConvertToPattern
    , I $ PrimitiveAnyPattern
    , I $ Return
    ]

apply :: Primitive
apply = Primitive "apply"
    [ I $ GetArgCount
    , I $ CallDynamic
    , I $ Return
    ]

array :: Primitive
array = Primitive "array"
    -- Get the number of args
    [ I $ GetArgCount
    -- Push two copies, we need to do 3 checks on it
    , I $ Copy 2
    -- Branch on if there are 0, 1 or 2, or more args
    , I $ PushInteger 0
    , I $ Equal
    , I'  JumpStaticIf "array.0"
    , I $ PushInteger 1
    , I $ Equal
    , I'  JumpStaticIf "array.1"
    , I $ PushInteger 2
    , I $ Equal
    , I' JumpStaticIf "array.2"
    
    -- If there are more than 2 arguments, we panic
    , I $ Panic IncorrectNumberOfArguments
    
    -- If there are no arguments, we panic
    , L   "array.0" 
        $ Panic NullStringInIllegalContext
    
    -- If there is only 1 arg, we push a null string to act as the initial value
    -- of the array before calling the array allocator.
    -- Before this, we pop off the remaining copy of the argument counter
    , L   "array.1"
        $ Pop
    , I $ PushString nullString
    , I $ AllocArray
    , I $ Return

    -- If there are two args, we simply call the array allocator
    , L   "array.2"
        $ AllocArray
    , I $ Return
    ]
    
