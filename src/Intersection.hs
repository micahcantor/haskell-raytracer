module Intersection where

import qualified Data.SortedList as SL
import Ray (Ray (..), position, transform)
import Sphere (Sphere (..), normalAt, unitSphere)
import Test.HUnit (Assertion, Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)
import Transformation (inverse, scaling, translation)
import VecPoint (Point (Point), Vec (Vec), dot, pSub, vNeg)

data Intersection = Intersection Float Sphere -- t value, intersected object
  deriving (Show, Eq)

instance Ord Intersection where
  (Intersection t1 _) <= (Intersection t2 _) = t1 <= t2

type Intersections = SL.SortedList Intersection

headSL :: Intersections -> Intersection
headSL xs = head $ SL.fromSortedList xs

atSL :: Intersections -> Int -> Intersection
xs `atSL` i = head $ SL.fromSortedList $ SL.drop i xs

hit :: Intersections -> Maybe Intersection
-- returns the first nonzero intersection, if it exists
hit xs =
  let zeroIntersection = Intersection 0 unitSphere
   in fmap fst $ SL.uncons $ SL.filterGE zeroIntersection xs

sphereIntersect :: Sphere -> Ray -> Intersections
sphereIntersect s@(Sphere center r t _) ray
  | d < 0 = SL.toSortedList []
  | otherwise = SL.toSortedList [t1, t2]
  where
    (Ray origin direction) = transform ray (inverse t)
    sphereToRay = origin `pSub` center
    a = direction `dot` direction
    b = 2 * (direction `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1
    d = (b ^ 2) - (4 * a * c)
    t1 = Intersection (((- b) - sqrt d) / (2 * a)) s
    t2 = Intersection (((- b) + sqrt d) / (2 * a)) s

data Computation = Computation Bool Float Sphere Point Vec Vec deriving (Show, Eq)
-- inside, t, object, point, eye, normal

prepareComputation :: Ray -> Intersection -> Computation
-- precomputes the state of an intersection
prepareComputation r@(Ray origin direction) (Intersection t object) =
  let point = position r t
      eyev = vNeg direction
      normalv = normalAt object point
      normalDotEye = normalv `dot` eyev
      newNormalv
        | normalDotEye < 0 = vNeg normalv
        | otherwise = normalv
      inside = normalDotEye < 0
   in Computation inside t object point eyev newNormalv

{- Tests -}
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