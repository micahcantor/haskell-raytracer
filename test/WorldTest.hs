module WorldTest (tests) where

import Data.SortedList as SL (fromSortedList)
import Intersection (prepareComputation)
import Shape (defaultSphere, defaultPlane)
import Test.HUnit (Test (..), assertEqual, assertString)
import Transformation (translation)
import Types
  ( Color (Color),
    Intersection (Intersection),
    Point (Point),
    PointLight (PointLight),
    Ray (Ray),
    Shape (..),
    Material(..),
    Vec (Vec),
    World (..),
  )
import Material ( defaultMaterial, black )
import World (colorAt, defaultWorld, intersect, isShadowed, shadeHit, reflectedColor, maxRecursions)

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
  assertEqual "equality" (Color 0.38066 0.47583 0.2855) (shadeHit w comps maxRecursions)

testShadeHit_ :: String
testShadeHit_ =
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = head $ objects w
      i = Intersection 4 shape
      comps = prepareComputation r i
   in show (shadeHit w comps maxRecursions )

testShadeHitInside :: Test
testShadeHitInside = TestCase $ do
  let w = defaultWorld {lights = [PointLight (Point 0 0.25 0) (Color 1 1 1)]}
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = objects w !! 1
      i = Intersection 0.5 shape
      comps = prepareComputation r i
  assertEqual "equality" (Color 0.90498 0.90498 0.90498) (shadeHit w comps maxRecursions)

testShadeHitInShadow :: Test
testShadeHitInShadow = TestCase $ do
  let s1 = defaultSphere
      s2 = defaultSphere {transform = translation 0 0 10}
      w = defaultWorld {lights = [PointLight (Point 0 0 (-10)) (Color 1 1 1)], objects = [s1, s2]}
      r = Ray (Point 0 0 5) (Vec 0 0 1)
      i = Intersection 4 s2
      comps = prepareComputation r i
  assertEqual "in shadow" (Color 0.1 0.1 0.1) (shadeHit w comps maxRecursions)

testShadeHitReflective :: Test
testShadeHitReflective = TestCase $ do
  let shape = defaultPlane {material = defaultMaterial {reflective = 0.5}, transform = translation 0 (-1) 0}
      w = defaultWorld {objects = [shape]}
      r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      i = Intersection (sqrt 2) shape
      comps = prepareComputation r i
  assertEqual "shading reflective material" (Color 0.87677 0.92436 0.82918) (shadeHit w comps maxRecursions)

testColorAtMiss :: Test
testColorAtMiss = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 1 0)
  assertEqual "miss" (Color 0 0 0) (colorAt w r maxRecursions)

testColorAtHit :: Test
testColorAtHit = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
  assertEqual "hit" (Color 0.38066 0.47583 0.2855) (colorAt w r maxRecursions)

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

testReflectedColorNonReflective :: Test
testReflectedColorNonReflective = TestCase $ do
  let w@(World _ (s1 : s2 : _)) = defaultWorld 
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = s2 {material = defaultMaterial {ambient = 1}}
      i = Intersection 1 shape
      comps = prepareComputation r i
  assertEqual "reflected color for nonreflective" black (reflectedColor w comps maxRecursions)

testReflectedColorReflective :: Test
testReflectedColorReflective = TestCase $ do
  let r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      shape = defaultPlane {material = defaultMaterial {reflective = 0.5}, transform = translation 0 (-1) 0}
      i = Intersection (sqrt 2) shape
      w = defaultWorld {objects = [shape]}
      comps = prepareComputation r i
  assertEqual "reflected color for reflective surface" (Color 0.19032 0.2379 0.14274) (reflectedColor w comps maxRecursions)

testReflectedColorLimitedRecursion :: Test
testReflectedColorLimitedRecursion = TestCase $ do
  let r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      shape = defaultPlane {material = defaultMaterial {reflective = 0.5}, transform = translation 0 (-1) 0}
      i = Intersection (sqrt 2) shape
      w = defaultWorld {objects = [shape]}
      comps = prepareComputation r i
      remaining = 0
  assertEqual "reflected color with 0 remaining recursions" (Color 0.19032 0.2379 0.14274) (reflectedColor w comps remaining)

{- testColorAtMutuallyReflective :: Test
testColorAtMutuallyReflective = TestCase $ do
  let lower = defaultPlane {material = defaultMaterial {reflective = 1}, transform = translation 0 (-1) 0}
      upper = defaultPlane {material = defaultMaterial {reflective = 1}, transform = translation 0 1 0}
      w = defaultWorld {lights = [PointLight (Point 0 0 0) (Color 1 1 1)], objects = [lower, upper]}
      r = Ray (Point 0 0 0) (Vec 0 1 0)
  assert t -}

tests :: Test
tests =
  TestList
    [ testIntersect,
      testShadeHit,
      testShadeHitInside,
      testShadeHitInShadow,
      testShadeHitReflective,
      testColorAtMiss,
      testColorAtHit,
      testIsShadowedNotColinear,
      testIsShadowedIsBehindSphere,
      testIsShadowedInFrontLight,
      testIsShadowedInFrontSphere,
      testReflectedColorNonReflective,
      testReflectedColorReflective
    ]