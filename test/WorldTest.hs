module WorldTest (tests) where

import Data.SortedList as SL (fromSortedList)
import Intersection (prepareComputation)
import Shape (defaultSphere)
import Test.HUnit (Test (..), assertEqual)
import Transformation (translation)
import Types
  ( Color (Color),
    Intersection (Intersection),
    Point (Point),
    PointLight (PointLight),
    Ray (Ray),
    Shape (transform),
    Vec (Vec),
    World (lights, objects),
  )
import World (colorAt, defaultWorld, intersect, isShadowed, shadeHit)

testIntersect :: Test
testIntersect = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      xs = w `intersect` r
  assertEqual "list equal" [4, 4.5, 5.5, 6] [t | (Intersection t _) <- SL.fromSortedList xs]

testShadeHit :: Test
testShadeHit = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = head $ objects w
      i = Intersection 4 shape
      comps = prepareComputation r i
  assertEqual "equality" (Color 0.38066 0.47583 0.2855) (shadeHit w comps)

testShadeHitInside :: Test
testShadeHitInside = TestCase $ do
  let w = defaultWorld {lights = [PointLight (Point 0 0.25 0) (Color 1 1 1)]}
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = objects w !! 1
      i = Intersection 0.5 shape
      comps = prepareComputation r i
  assertEqual "equality" (Color 0.90498 0.90498 0.90498) (shadeHit w comps)

testShadeHitInShadow :: Test
testShadeHitInShadow = TestCase $ do
  let s1 = defaultSphere
      s2 = defaultSphere {transform = translation 0 0 10}
      w = defaultWorld {lights = [PointLight (Point 0 0 (-10)) (Color 1 1 1)], objects = [s1, s2]}
      r = Ray (Point 0 0 5) (Vec 0 0 1)
      i = Intersection 4 s2
      comps = prepareComputation r i
  assertEqual "in shadow" (Color 0.1 0.1 0.1) (shadeHit w comps)

testColorAtMiss :: Test
testColorAtMiss = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 1 0)
  assertEqual "miss" (Color 0 0 0) (colorAt w r)

testColorAtHit :: Test
testColorAtHit = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
  assertEqual "hit" (Color 0.38066 0.47583 0.2855) (colorAt w r)

testIsShadowedNotColinear :: Test
testIsShadowedNotColinear = TestCase $ do
  let w = defaultWorld
      p = Point 0 10 0
      light = head (lights w)
  assertEqual "not colinear" False (isShadowed w p light)

testIsShadowedIsBehindSphere :: Test
testIsShadowedIsBehindSphere = TestCase $ do
  let w = defaultWorld
      p = Point 10 (-10) 10
      light = head (lights w)
  assertEqual "is between sphere and light" True (isShadowed w p light)

testIsShadowedInFrontLight :: Test
testIsShadowedInFrontLight = TestCase $ do
  let w = defaultWorld
      p = Point (-20) 20 (-20)
      light = head (lights w)
  assertEqual "is in front of light" False (isShadowed w p light)

testIsShadowedInFrontSphere :: Test
testIsShadowedInFrontSphere = TestCase $ do
  let w = defaultWorld
      p = Point (-2) 2 (-2)
      light = head (lights w)
  assertEqual "is in front of sphere" False (isShadowed w p light)

tests :: Test
tests =
  TestList
    [ testIntersect,
      testShadeHit,
      testShadeHitInside,
      testShadeHitInShadow,
      testColorAtMiss,
      testColorAtHit,
      testIsShadowedNotColinear,
      testIsShadowedIsBehindSphere,
      testIsShadowedInFrontLight,
      testIsShadowedInFrontSphere
    ]