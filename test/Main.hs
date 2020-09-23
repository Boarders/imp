module Main where

import           IMP.Cases
import           IMP.Eval
import           IMP.Syntax            (Value (..))
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      as HU
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]


properties :: TestTree
properties =
  testGroup "Property Tests"
    [ QC.testProperty
        "Sum from 1 to n" $
         withMaxSuccess 1000 $ forAll (genArgs 1000) gaussSumTest
    , QC.testProperty
        "Collatz Total Steps" $
         withMaxSuccess 200 $ forAll (genArgs 200) collatzTest
    , QC.testProperty
        "Number of Primes" $
         withMaxSuccess 200 $ forAll (genArgs 200) primesTest
    ]


unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase
      "sum    : s should be 5050 when n is 100" $
       evalProgVar "s" (gaussSum 100) @?= Just (VInt 5050)
  , HU.testCase
      "primes : s should be 4 when m = 10" $
       evalProgVar "s" (primes 10) @?= Just (VInt 4)
  , HU.testCase
      "collatz: s should be 66 when m is 10" $
       evalProgVar "s" (collatz 10) @?= Just (VInt 66)
  ]


gaussSumTest :: Int -> Property
gaussSumTest n =
  Just (VInt (gaussSumRef n)) === evalProgVar "s" (gaussSum n)

collatzTest :: Int -> Property
collatzTest n =
  Just (VInt (collatzRef n)) === evalProgVar "s" (collatz n)

primesTest :: Int -> Property
primesTest n =
  Just (VInt (primesRef n)) === evalProgVar "s" (primes n)


genArgs :: Int -> Gen Int
genArgs upper = choose (2, upper)
