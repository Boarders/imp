{-# LANGUAGE GADTs #-}

module IMP.Syntax where

type Id = String

data Prim where
  PrimVar  :: String -> Prim
  PrimInt  :: Int    -> Prim
  PrimNeg  :: Prim   -> Prim
  PrimAdd  :: Prim   -> Prim -> Prim
  PrimDiv  :: Prim   -> Prim -> Prim
  deriving (Show)

data BoolExpr where
  Bool :: Bool   -> BoolExpr
  LEQ  :: Prim -> Prim -> BoolExpr
  Not  :: BoolExpr -> BoolExpr
  And  :: BoolExpr -> BoolExpr -> BoolExpr
  deriving (Show)

data Statement where
  Decl  :: Id        -> Prim            -> Statement
  If    :: BoolExpr  -> Block -> Block  -> Statement
  While :: BoolExpr  -> Block           -> Statement
  Seq   :: Statement -> Statement       -> Statement
  deriving (Show)

data Block where
  EmptyBlock     :: Block
  StatementBlock :: Statement -> Block
  deriving (Show)


data Value where
  VInt  :: Int  -> Value
  VBool :: Bool -> Value
  deriving (Eq, Show)
