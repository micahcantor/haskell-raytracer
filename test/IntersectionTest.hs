module IntersectionTest (tests) where

import qualified Data.SortedList as SL
import Intersection
  ( Computation (Computation),
    Intersection (Intersection),
    atSL,
    headSL,
    hit,
    prepareComputation,
    sphereIntersect,
  )
import Ray (Ray (Ray))
import Sphere (Sphere (transformation), unitSphere)
import Test.HUnit (Test (..), assertEqual)
import Transformation (scaling, translation)
import VecPoint (Point (Point), Vec (Vec))

testHitPos :: Test
testHitPos = TestCase $ do
  let s = unitSphere
      i1 = Intersection 1 s
      i2 = Intersection 2 s
      xs = SL.toSortedList [i1, i2]
  assertEqual "first" (hit xs) (Just i1)

testHitSomeNeg :: Test
testHitSomeNeg = TestCase $ do
  let s = unitSphere
      i1 = Intersection (-1) s
      i2 = Intersection 2 s
      xs = SL.toSortedList [i1, i2]
  assertEqual "second" (hit xs) (Just i2)

testHitAllNeg :: Test
testHitAllNeg = TestCase $ do
  let s = unitSphere
      i1 = Intersection (-1) s
      i2 = Intersection (-2) s
      xs = SL.toSortedList [i1, i2]
  assertEqual "nothing" (hit xs) Nothing

testHitMix :: Test
testHitMix = TestCase $ do
  let s = unitSphere
      i1 = Intersection 5 s
      i2 = Intersection 7 s
      i3 = Intersection (-3) s
      i4 = Intersection 2 s
      xs = SL.toSortedList [i1, i2, i3, i4]
  assertEqual "fourth" (hit xs) (Just i4)

testIntersectionTangent :: Test
testIntersectionTangent = TestCase $ do
  let r = Ray (Point 0 1 (-5)) (Vec 0 0 1)
      s = unitSphere
      xs = s `sphereIntersect` r
  assertEqual "first" (Intersection 5.0 s) (headSL xs)
  assertEqual "second" (Intersection 5.0 s) (xs `atSL` 1)

testIntersectionInside :: Test
testIntersectionInside = TestCase $ do
  let r = Ray (Point 0 0 0) (Vec 0 0 1)
      s = unitSphere
      xs = s `sphereIntersect` r
  assertEqual "first" (Intersection (-1.0) s) (headSL xs)
  assertEqual "second" (Intersection 1.0 s) (xs `atSL` 1)

testIntersectionBehind :: Test
testIntersectionBehind = TestCase $ do
  let r = Ray (Point 0 0 5) (Vec 0 0 1)
      s = unitSphere
      xs = s `sphereIntersect` r
  assertEqual "first" (Intersection (-6.0) s) (headSL xs)
  assertEqual "second" (Intersection (-4.0) s) (xs `atSL` 1)

testIntersectionScaled :: Test
testIntersectionScaled = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      s = unitSphere {transformation = scaling 2 2 2}
      xs = s `sphereIntersect` r
  assertEqual "first" (Intersection 3 s) (headSL xs)
  assertEqual "second" (Intersection 7 s) (xs `atSL` 1)

testIntersectionTranslated :: Test
testIntersectionTranslated = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      s = unitSphere {transformation = translation 5 0 0}
      xs = s `sphereIntersect` r
  assertEqual "length" (length xs) 0

testPrepareComputation :: Test
testPrepareComputation = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = unitSphere
      i@(Intersection t object) = Intersection 4 shape
  assertEqual
    "equality"
    (Computation False t object (Point 0 0 (-1)) (Vec 0 0 (-1)) (Vec 0 0 (-1)))
    (prepareComputation r i)

testPrepareComputationOutside :: Test
testPrepareComputationOutside = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = unitSphere
      i = Intersection 4 shape
      (Computation inside _ _ _ _ _) = prepareComputation r i
  assertEqual "equality" False inside

testPrepareComputationInside :: Test
testPrepareComputationInside = TestCase $ do
  let r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = unitSphere
      i@(Intersection _ object) = Intersection 1 shape
      (Computation inside t obj point eye normal) = prepareComputation r i
  assertEqual
    "equality"
    (Computation True t object (Point 0 0 1) (Vec 0 0 (-1)) (Vec 0 0 (-1)))
    (prepareComputation r i)

tests :: Test
tests =
  TestList
    [ testIntersectionInside,
      testIntersectionTangent,
      testIntersectionBehind,
      testIntersectionScaled,
      testIntersectionTranslated,
      testHitPos,
      testHitSomeNeg,
      testHitAllNeg,
      testHitMix,
      testPrepareComputation,
      testPrepareComputationOutside,
      testPrepareComputationInside
    ]
