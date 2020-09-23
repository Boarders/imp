{-# LANGUAGE LambdaCase   #-}

module IMP.Eval where

import           IMP.Syntax

import           Control.Monad              (guard)
import           Control.Monad.State.Strict (StateT, execStateT, get, modify')
import           Control.Monad.Trans.Class  (lift)
import           Data.List.NonEmpty
import qualified Data.Map                   as Map


type Store  = Map.Map Id Value
type EvalM a = StateT Store Maybe a


initialStore :: Store
initialStore = mempty

lookupStore :: Id -> Store -> Maybe Value
lookupStore = Map.lookup

setStore :: Id -> Value -> Store -> Store
setStore = Map.insert

initialiseVars :: NonEmpty Id -> Statement
initialiseVars =
  foldr1 Seq . fmap (\i -> Decl i (PrimInt 0))

evalPrim :: Prim -> EvalM Value
evalPrim = \case
  PrimVar v     ->
    get >>= \store -> lift $ lookupStore v store
  PrimInt n     -> pure . VInt $ n
  PrimNeg n     -> do
 -- Note: We match against the constructor for VInt assuming
 -- the program we receive is type correct and thus that
 -- the result of evaluation is known to be an integer.
 -- If we accept type incorrect programs then we may
 -- also handle these cases.
    (VInt v) <- evalPrim n
    pure . VInt $ (- v)
  PrimAdd n1 n2 -> do
 -- See note for PrimNeg.
    (VInt v1) <- evalPrim n1
    (VInt v2) <- evalPrim n2
    pure . VInt $ v1 + v2

  PrimDiv n1 n2 -> do
 -- See note for PrimNeg.
    (VInt v1) <- evalPrim n1
    (VInt v2) <- evalPrim n2
    guard (v2 /= 0)
    pure . VInt $ v1 `div` v2


evalBool :: BoolExpr -> EvalM Value
evalBool = \case
  Bool b -> pure . VBool $ b
  LEQ i1 i2 -> do
 -- See note for PrimNeg.
    VInt iv1 <- evalPrim i1
    VInt iv2 <- evalPrim i2
    pure . VBool $ (iv1 <= iv2)
  Not b -> do
    VBool bv <- evalBool b
    pure . VBool $ not bv
  And b1 b2 -> do
 -- See note for PrimNeg.
    VBool vb1 <- evalBool b1
    VBool vb2 <- evalBool b2
    pure . VBool $ vb1 && vb2

evalStatement :: Statement -> EvalM ()
evalStatement = \case
  Decl var p -> do
    val <- evalPrim p
    modify' (setStore var val)

  If b th el -> do
 -- see note from PrimNeg.
    (VBool bv) <- evalBool b
    if bv
      then evalBlock th
      else evalBlock el

  while@(While b bl) -> do
 -- see note from PrimNeg.
    (VBool bv) <- evalBool b
    if bv
      then evalBlock bl >> evalStatement while
      else pure ()
  Seq s1 s2  ->
    evalStatement s1 >> evalStatement s2

evalBlock :: Block -> EvalM ()
evalBlock  = \case
  EmptyBlock       -> pure ()
  StatementBlock s -> evalStatement s


evalProgVar :: Id -> Block -> Maybe Value
evalProgVar var block = do
  finalStore <- flip execStateT initialStore $ evalBlock block
  lookupStore var finalStore
