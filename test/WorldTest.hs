module WorldTest (tests) where

import Test.HUnit (Test(..), assertEqual)
import Data.SortedList as SL (fromSortedList)
import Intersection
    ( Intersection(Intersection), prepareComputation )
import VecPoint ( Point(Point), Vec(Vec) )
import Light ( PointLight(PointLight) )
import Ray ( Ray(Ray) )
import Color ( Color(Color) )
import World
    ( colorAt,
      defaultWorld,
      intersect,
      shadeHit,
      World(lights, objects) )

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

tests :: Test
tests =
  TestList
    [ testIntersect,
      testShadeHit,
      testShadeHitInside,
      testColorAtMiss,
      testColorAtHit
    ]