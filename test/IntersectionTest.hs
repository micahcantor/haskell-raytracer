module IntersectionTest (tests) where

import qualified Data.SortedList as SL
import Intersection (atSL, headSL, hit, prepareComputation)
import Shape (defaultPlane, defaultSphere, glassSphere, intersect)
import Test.HUnit (Test (..), assertBool, assertEqual)
import Transformation (scaling, translation)
import Types
  ( Computation (..),
    Intersection (Intersection),
    Material (..),
    Point (Point),
    Ray (Ray),
    Shape (material, transform),
    Vec (Vec),
    toIntersections,
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
      (Computation inside compT compObj point eye normal _ _ _ _ _) = prepareComputation r i (toIntersections [i])
  assertEqual
    "equality"
    (False, t, object, Point 0 0 (-1), Vec 0 0 (-1), Vec 0 0 (-1))
    (inside, compT, compObj, point, eye, normal)

testPrepareComputationOutside :: Test
testPrepareComputationOutside = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = defaultSphere
      i = Intersection 4 shape
      ins = inside $ prepareComputation r i (toIntersections [i])
  assertEqual "equality" False ins

testPrepareComputationInside :: Test
testPrepareComputationInside = TestCase $ do
  let r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = defaultSphere
      i@(Intersection _ object) = Intersection 1 shape
      (Computation inside t obj point eye normal _ _ _ _ _) = prepareComputation r i (toIntersections [i])
  assertEqual
    "equality"
    (True, t, object, Point 0 0 1, Vec 0 0 (-1), Vec 0 0 (-1))
    (inside, t, obj, point, eye, normal)

testPrepareComputationOffset :: Test
testPrepareComputationOffset = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = defaultSphere {transform = translation 0 0 1}
      i = Intersection 5 shape
      comps = prepareComputation r i (toIntersections [i])
      (Point _ _ overZ) = over comps
      (Point _ _ intersectZ) = point comps
  assertBool
    "the hit should offset the point"
    (overZ < (- epsilon) / 2 && intersectZ > overZ)

testPrepareComputationReflection :: Test
testPrepareComputationReflection = TestCase $ do
  let shape = defaultPlane
      r = Ray (Point 0 1 (-1)) (Vec 0 ((- sqrt 2) / 2) (sqrt 2 / 2))
      i = Intersection (sqrt 2) shape
      reflectv = reflect (prepareComputation r i (toIntersections [i]))
  assertEqual "precomputing the reflection vector" (Vec 0 (sqrt 2 / 2) (sqrt 2 / 2)) reflectv

testFindN1AndN2 :: Test
testFindN1AndN2 = TestCase $ do
  let a = glassSphere {transform = scaling 2 2 2}
      b = glassSphere {transform = translation 0 0 (-0.25), material = (material glassSphere) {refractiveIndex = 2.0}}
      c = glassSphere {transform = translation 0 0 0.25, material = (material glassSphere) {refractiveIndex = 2.5}}
      r = Ray (Point 0 0 (-4)) (Vec 0 0 1)
      xs = toIntersections $ map (uncurry Intersection) [(2, a), (2.75, b), (3.25, c), (4.75, b), (5.25, c), (6, a)]
      allComps = map (\i -> prepareComputation r i xs) (SL.fromSortedList xs)
      (n1s, n2s) = (map n1 allComps, map n2 allComps)
  assertEqual "finding n1 at various intersections" [1.0, 1.5, 2.0, 2.5, 2.5, 1.5] n1s
  assertEqual "finding n2 at various intersections" [1.5, 2.0, 2.5, 2.5, 1.5, 1.0] n2s

testComputeUnderPoint :: Test
testComputeUnderPoint = TestCase $ do
  let r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = glassSphere {transform = translation 0 0 1}
      i = Intersection 5 shape
      xs = toIntersections [i]
      comps = prepareComputation r i xs
      (Point _ _ underZ) = under comps
      (Point _ _ pointZ) = point comps
  assertBool "under point goes under" (underZ > epsilon / 2)
  assertBool "uner point is under surface" (pointZ < underZ)

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
      testPrepareComputationOffset,
      testPrepareComputationReflection,
      testFindN1AndN2,
      testComputeUnderPoint
    ]
