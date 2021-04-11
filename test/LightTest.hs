module LightTest (tests) where

import Color (Color (Color))
import Light (PointLight (PointLight), lighting)
import Material (defaultMaterial)
import Test.HUnit (Test (..), assertEqual)
import VecPoint (Point (Point), Vec (Vec))

testLightingBetween :: Test
testLightingBetween = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      inShadow = False
      result = lighting m light pos eyev normalv inShadow
  assertEqual "between" (Color 1.9 1.9 1.9) result

testLightingBetween45 :: Test
testLightingBetween45 = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 (sqrt 2 / 2) (- sqrt 2 / 2)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      inShadow = False
      result = lighting m light pos eyev normalv inShadow
  assertEqual "between 45" (Color 1.0 1.0 1.0) result

testLightingOpposite45 :: Test
testLightingOpposite45 = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
      inShadow = False
      result = lighting m light pos eyev normalv inShadow
  assertEqual "opposite 45" (Color 0.7364 0.7364 0.7364) result

testLightingInPath :: Test
testLightingInPath = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 (- sqrt 2 / 2) (- sqrt 2 / 2)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
      inShadow = False
      result = lighting m light pos eyev normalv inShadow
  assertEqual "in path" (Color 1.6364 1.6364 1.6364) result

testLightingBehind :: Test
testLightingBehind = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 10) (Color 1 1 1)
      inShadow = False
      result = lighting m light pos eyev normalv inShadow
  assertEqual "behind" (Color 0.1 0.1 0.1) result

testLightingInShadow :: Test
testLightingInShadow = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      inShadow = True
      result = lighting m light pos eyev normalv inShadow
  assertEqual "between" (Color 1.9 1.9 1.9) result

tests :: Test
tests =
  TestList
    [ testLightingBetween,
      testLightingBetween45,
      testLightingOpposite45,
      testLightingInPath,
      testLightingBehind,
      testLightingInShadow
    ]