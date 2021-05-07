module VecPointTest where

import Test.HUnit (Test (..), assertEqual)
import VecPoint (reflect)
import Types ( Vec(Vec) )

testReflect :: Test
testReflect = TestCase $ do
  let v1 = Vec 0 (-1) 0
      n1 = Vec (sqrt 2 / 2) (sqrt 2 / 2) 0
      r1 = reflect v1 n1
      v2 = Vec 1 (-1) 0
      n2 = Vec 0 1 0
      r2 = reflect v2 n2
  assertEqual "slanted" (Vec 1 0 0) r1
  assertEqual "45 deg" (Vec 1 1 0) r2

tests :: Test
tests = TestList [testReflect]