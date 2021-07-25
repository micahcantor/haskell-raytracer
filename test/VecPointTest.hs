module VecPointTest where

import Test.HUnit (Test (..), assertEqual)
import VecPoint (reflect, vpAdd, pSub, vpSub, vSub, vNeg, pMult, pDiv, magnitude, normalize, dot, cross)
import Types ( Vec(Vec), Point (Point) )

testAdd :: Test 
testAdd = TestCase $ do
  let p = Point 3 (-2) 5
      v = Vec (-2) 3 1
  assertEqual "add point and vec" (p `vpAdd` v) (Point 1 1 6)

testSub :: Test 
testSub = TestCase $ do
  let p1 = Point 3 2 1
      p2 = Point 5 6 7
      v1 = Vec 3 2 1
      v2 = Vec 5 6 7
  assertEqual "sub two points" (p1 `pSub` p2) (Vec (-2) (-4) (-6))
  assertEqual "sub point and vec" (p1 `vpSub` v2) (Point (-2) (-4) (-6))
  assertEqual "sub two vecs" (v1 `vSub` v2) (Vec (-2) (-4) (-6))

testNegate :: Test 
testNegate = TestCase $ do
  let v = Vec 1 (-2) 3
  assertEqual "negate a vec" (vNeg v) (Vec (-1) 2 (-3))

testScalarOp :: Test 
testScalarOp = TestCase $ do
  let a = Point 1 (-2) 3
  assertEqual "mult by scalar" (3.5 `pMult` a) (Point 3.5 (-7) 10.5)
  assertEqual "div by scalar" (a `pDiv` 2) (Point 0.5 (-1) 1.5)

testMagnitude :: Test
testMagnitude = TestCase $ do
  let v1 = Vec 1 0 0
      v2 = Vec 0 1 0
      v3 = Vec 1 2 3
  assertEqual "v1 mag" (magnitude v1) 1
  assertEqual "v2 mag" (magnitude v2) 1
  assertEqual "v3 mag" (magnitude v3) (sqrt 14)

testNormalize :: Test 
testNormalize = TestCase $ do
  let v1 = Vec 4 0 0
      v2 = Vec 1 2 3
  assertEqual "normalize v1" (normalize v1) (Vec 1 0 0)
  assertEqual "normalize v2" (normalize v2) (Vec (1 / sqrt 14) (2 / sqrt 14) (3 / sqrt 14))
  assertEqual "mag of normalized" (magnitude $ normalize v2) 1

testDotAndCross :: Test 
testDotAndCross = TestCase $ do
  let a = Vec 1 2 3
  let b = Vec 2 3 4
  assertEqual "a dot b" (a `dot` b) 20
  assertEqual "a cross b" (a `cross` b) (Vec (-1) 2 (-1))
  assertEqual "b cross a" (b `cross` a) (Vec 1 (-2) 1)

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
tests = TestList [testAdd, testSub, testNegate, testScalarOp, testMagnitude, testNormalize, testDotAndCross, testReflect]