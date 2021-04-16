module IntersectionTest (tests) where

import qualified Data.SortedList as SL
import Intersection ( headSL, atSL, hit, prepareComputation )
import Shape ( intersect, defaultSphere )
import Test.HUnit (Test (..), assertBool, assertEqual)
import Transformation (scaling, translation)
import Types
  ( Computation (Computation, over, point),
    Intersection (Intersection),
    Point (Point),
    Ray (Ray),
    Shape (transform),
    Vec (Vec),
  )
import VecPoint (epsilon)

testHitPos :: Test
testHitPos = TestCase $ do
  let s = defaultSphere
      i1 = Intersection 1 s
      i2 = Intersection 2 s
      xs = SL.toSortedList [i1, i2]
  assertEqual "first" (hit xs) (Just i1)

testHitSomeNeg :: Test
testHitSomeNeg = TestCase $ do
  let s = defaultSphere
      i1 = Intersection (-1) s
      i2 = Intersection 2 s
      xs = SL.toSortedList [i1, i2]
  assertEqual "second" (hit xs) (Just i2)

testHitAllNeg :: Test
testHitAllNeg = TestCase $ do
  let s = defaultSphere
      i1 = Intersection (-1) s
      i2 = Intersection (-2) s
      xs = SL.toSortedList [i1, i2]
  assertEqual "nothing" (hit xs) Nothing

testHitMix :: Test
testHitMix = TestCase $ do
  let s = defaultSphere
      i1 = Intersection 5 s
      i2 = Intersection 7 s
      i3 = Intersection (-3) s
      i4 = Intersection 2 s
      xs = SL.toSortedList [i1, i2, i3, i4]
  assertEqual "fourth" (hit xs) (Just i4)

testIntersectionTangent :: Test
testIntersectionTangent = TestCase $ do
  let r = Ray (Point 0 1 (-5)) (Vec 0 0 1)
      s = defaultSphere
      xs = s `intersect` r
  assertEqual "first" (Intersection 5.0 s) (headSL xs)
  assertEqual "second" (Intersection 5.0 s) (xs `atSL` 1)

testIntersectionInside :: Test
testIntersectionInside = TestCase $ do
  let r = Ray (Point 0 0 0) (Vec 0 0 1)
      s = defaultSphere
      xs = s `intersect` r
  assertEqual "first" (Intersection (-1.0) s) (headSL xs)
  assertEqual "second" (Intersection 1.0 s) (xs `atSL` 1)

testIntersectionBehind :: Test
testIntersectionBehind = TestCase $ do
  let r = Ray (Point 0 0 5) (Vec 0 0 1)
      s = defaultSphere
      xs = s `intersect` r
  assertEqual "first" (Intersection (-6.0) s) (headSL xs)
  assertEqual "second" (Intersection (-4.0) s) (xs `atSL` 1)

testIntersectionScaled :: Test
testIntersectionScaled = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      s = defaultSphere {transform = scaling 2 2 2}
      xs = s `intersect` r
  assertEqual "first" (Intersection 3 s) (headSL xs)
  assertEqual "second" (Intersection 7 s) (xs `atSL` 1)

testIntersectionTranslated :: Test
testIntersectionTranslated = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      s = defaultSphere {transform = translation 5 0 0}
      xs = s `intersect` r
  assertEqual "length" (length xs) 0

testPrepareComputation :: Test
testPrepareComputation = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = defaultSphere
      i@(Intersection t object) = Intersection 4 shape
      (Computation inside compT compObj point eye normal _) = prepareComputation r i
  assertEqual
    "equality"
    (False, t, object, Point 0 0 (-1), Vec 0 0 (-1), Vec 0 0 (-1))
    (inside, compT, compObj, point, eye, normal)

testPrepareComputationOutside :: Test
testPrepareComputationOutside = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = defaultSphere
      i = Intersection 4 shape
      (Computation inside _ _ _ _ _ _) = prepareComputation r i
  assertEqual "equality" False inside

testPrepareComputationInside :: Test
testPrepareComputationInside = TestCase $ do
  let r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = defaultSphere
      i@(Intersection _ object) = Intersection 1 shape
      (Computation inside t obj point eye normal _) = prepareComputation r i
  assertEqual
    "equality"
    (True, t, object, Point 0 0 1, Vec 0 0 (-1), Vec 0 0 (-1))
    (inside, t, obj, point, eye, normal)

testPrepareComputationOffset :: Test
testPrepareComputationOffset = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = defaultSphere {transform = translation 0 0 1}
      i = Intersection 5 shape
      comps = prepareComputation r i
      (Point _ _ overZ) = over comps
      (Point _ _ intersectZ) = point comps
  assertBool
    "the hit should offset the point"
    (overZ < (- epsilon) / 2 && intersectZ > overZ)

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
      testPrepareComputationInside,
      testPrepareComputationOffset
    ]
