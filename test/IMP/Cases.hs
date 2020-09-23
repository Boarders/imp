module IMP.Cases where

import           IMP.Eval
import           IMP.Syntax

import           Data.List.NonEmpty (NonEmpty (..))


collatzRef :: Int -> Int
collatzRef input = go 0 input input
  where
    go :: Int -> Int -> Int -> Int
    go s curr tot | tot == 2 = s
    go s curr tot | curr == 1 = go s (tot - 1) (tot - 1)
    go s curr tot | even curr = go (s + 1) (curr `div` 2) tot
    go s curr tot = go (s + 1) (3 * curr + 1) tot

collatz :: Int -> Block
collatz input = StatementBlock progStatements
  where
    progStatements =
      foldr1 Seq
        [initialise, initM, outerLoop]
    m, n, q, r, s :: Id
    m = "m"
    n = "n"
    q = "q"
    r = "r"
    s = "s"

    initialise :: Statement
    initialise = initialiseVars (m :| [n, q, r, s])

    initM :: Statement
    initM = Decl m (PrimInt input)

    outerLoop :: Statement
    outerLoop =
      let
        cond = Not $ PrimVar m `LEQ` PrimInt 2
        loop = StatementBlock $
          foldr1 Seq
          [ Decl n (PrimVar m)
          , Decl m (PrimAdd (PrimVar m) (PrimInt (- 1)))
          , innerLoop
          ]
      in
        While cond loop

    innerLoop :: Statement
    innerLoop =
      let
        cond = Not $ PrimVar n `LEQ` PrimInt 1
        loop = StatementBlock $
          foldr1 Seq
          [ Decl s (PrimAdd (PrimVar s) (PrimInt 1))
          , Decl q (PrimDiv (PrimVar n) (PrimInt 2))
          , Decl r (PrimAdd (PrimVar q) (PrimAdd (PrimVar q) (PrimInt 1)))
          , branch
          ]
      in
        While cond loop

    branch :: Statement
    branch =
      let
        boolExpr = PrimVar r `LEQ` PrimVar n
        thenBr   =
          StatementBlock $
            Decl n
              (PrimAdd
                (PrimVar n)
                  (PrimAdd
                     (PrimVar n)
                     (PrimAdd
                       (PrimVar n) (PrimInt 1))))
        elseBr   =
          StatementBlock $
            Decl n (PrimVar q)
      in
        If boolExpr thenBr elseBr


gaussSumRef :: Int -> Int
gaussSumRef n = sum [1..n]

gaussSum :: Int -> Block
gaussSum input = StatementBlock progStatements
  where
    progStatements =
      foldr1 Seq [initialise, nInit, sInit, loop]
    n, s :: Id
    n = "n"
    s = "s"

    initialise :: Statement
    initialise = initialiseVars (n :| [s])

    nInit :: Statement
    nInit = Decl n (PrimInt input)

    sInit :: Statement
    sInit = Decl s (PrimInt 0)

    loop :: Statement
    loop =
      let
        cond  = Not $ PrimVar n `LEQ` PrimInt 0
        instr =
          StatementBlock
            $ foldr1 Seq
                [ Decl s (PrimAdd (PrimVar s) (PrimVar n))
                , Decl n (PrimAdd (PrimVar n) (PrimInt (- 1)))
                ]
      in
        While cond instr


primesRef :: Int -> Int
primesRef m = length [ i | i <- [2..m], isPrime i]
  where
    isPrime :: Int -> Bool
    isPrime n =
      let
        go :: Int -> Bool
        go i | i > floor (sqrt (fromIntegral n)) = True
        go i | n `rem` i == 0 = False
        go i = go (i + 1)
      in
        go 2

primes :: Int -> Block
primes input = StatementBlock progStatements
  where
    progStatements =
      foldr1 Seq [initialise, mInit, nInit, loop]
    i, m, n, q, r, s, t, x, y, z :: Id
    i = "i"
    m = "m"
    n = "n"
    q = "q"
    r = "r"
    s = "s"
    t = "t"
    x = "x"
    y = "y"
    z = "z"

    initialise :: Statement
    initialise = initialiseVars (i :| [m,n,n,q,r,s,t,x,y,z])

    mInit :: Statement
    mInit = Decl m (PrimInt input)

    nInit :: Statement
    nInit = Decl n (PrimInt 2)

    loop :: Statement
    loop =
      let
        cond  = PrimVar n `LEQ` PrimVar m
        instr =
          StatementBlock
            $ foldr1 Seq
                [ Decl i (PrimInt 2)
                , Decl q (PrimDiv (PrimVar n) (PrimVar i))
                , Decl t (PrimInt 1)
                , iLoop
                , countBranch
                , Decl n (PrimAdd (PrimVar n) (PrimInt 1))
                ]
      in
        While cond instr

    iLoop :: Statement
    iLoop =
      let
        cond  = (PrimVar i `LEQ` PrimVar q) `And` (PrimInt 1 `LEQ` PrimVar t)
        instr =
          StatementBlock
            $ foldr1 Seq
                [ Decl x (PrimVar i)
                , Decl y (PrimVar q)
                , Decl z (PrimInt 0)
                , multLoop
                , primalityBranch
                ]
      in
        While cond instr

    multLoop :: Statement
    multLoop =
      let
        cond  = Not $ PrimVar x `LEQ` PrimInt 0
        evenBranch =
          If (PrimVar r `LEQ` PrimVar x)
            (StatementBlock $ Decl z (PrimAdd (PrimVar z) (PrimVar y)))
            EmptyBlock
        instr =
          StatementBlock
            $ foldr1 Seq
                [ Decl q (PrimDiv (PrimVar x) (PrimInt 2))
                , Decl r (PrimAdd (PrimVar q) (PrimAdd (PrimVar q) (PrimInt 1)))
                , evenBranch
                , Decl x (PrimVar q)
                , Decl y (PrimAdd (PrimVar y) (PrimVar y))
                ]
      in
        While cond instr

    primalityBranch :: Statement
    primalityBranch =
      let
        cond   = PrimVar n `LEQ` PrimVar z
        thenBr = StatementBlock $ Decl t (PrimInt 0)
        elseBr = StatementBlock $
                   Decl i (PrimAdd (PrimVar i) (PrimInt 1)) `Seq`
                   Decl q (PrimDiv (PrimVar n) (PrimVar i))
      in
        If cond thenBr elseBr

    countBranch :: Statement
    countBranch =
      let
        cond   = PrimInt 1 `LEQ` PrimVar t
        thenBr = StatementBlock $ Decl s (PrimAdd (PrimVar s) (PrimInt 1))
        elseBr = EmptyBlock
      in
        If cond thenBr elseBr
