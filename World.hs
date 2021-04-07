module World where

import Color (Color (Color))
import Data.SortedList as SL (fromSortedList)
import Intersection (Computation (..), Intersection (Intersection), Intersections, hit, prepareComputation, sphereIntersect)
import Light (PointLight (..), lighting)
import Material (Material (..), defaultMaterial)
import Ray (Ray (Ray))
import Sphere (Sphere (..), unitSphere)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)
import Transformation (scaling)
import VecPoint (Point (Point), Vec (Vec))

data World = World {light :: PointLight, objects :: [Sphere]}

defaultWorld :: World
defaultWorld =
  let light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
      s1 = unitSphere {material = defaultMaterial {color = Color 0.8 1.0 0.6, diffuse = 0.7, specular = 0.2}}
      s2 = unitSphere {transformation = scaling 0.5 0.5 0.5}
   in World light [s1, s2]

intersect :: World -> Ray -> Intersections
-- combine the intersections of each object in world with ray
intersect (World _ objects) r = mconcat $ map (`sphereIntersect` r) objects

shadeHit :: World -> Computation -> Color
shadeHit (World light _) (Computation _ _ object point eyev normalv) =
  lighting (material object) light point eyev normalv

colorAt :: World -> Ray -> Color
colorAt w r =
  let black = Color 0 0 0
      intersections = w `intersect` r
      hitIntersection = hit intersections
   in case hitIntersection of
        Nothing -> black
        (Just intersection) -> shadeHit w (prepareComputation r intersection)

{- Tests -}
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
  let w = defaultWorld {light = PointLight (Point 0 0.25 0) (Color 1 1 1)}
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