module Light where

import Color (Color (..), cAdd, cMult, hadamard)
import Material (Material (..), color, defaultMaterial)
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)
import VecPoint (Point (..), Vec (..), dot, normalize, pSub, reflect, vNeg)

data PointLight = PointLight {position :: Point, intensity :: Color} 

lighting :: Material -> PointLight -> Point -> Vec -> Vec -> Color
lighting material light point eyev normalv =
  ambientLight `cAdd` diffuseLight `cAdd` specularLight
  where
    black = Color 0 0 0
    -- combine surface color and light's color
    effectiveColor = color material `hadamard` intensity light
    -- find the direction to the light source
    lightv = normalize (position light `pSub` point)
    -- compute the ambient contribution
    ambientLight = ambient material `cMult` effectiveColor
    -- lightDotNormal represents the cosine of the angle between the light vector
    -- and the normal vector. Negative value means light is on the other side of surface.
    lightDotNormal = lightv `dot` normalv
    diffuseLight
      | lightDotNormal < 0 = black
      | otherwise = (diffuse material * lightDotNormal) `cMult` effectiveColor
    specularLight
      | lightDotNormal < 0 = black
      | reflectDotEye < 0 = black
      | otherwise = (factor * specular material) `cMult` intensity light
      where
        reflectv = reflect (vNeg lightv) normalv
        -- reflectDotEye represents the cosine of the angle between the reflect vector
        -- and the eye vector. Negative value means light is on the other side of surface.
        reflectDotEye = reflectv `dot` eyev
        factor = reflectDotEye ** shininess material

{- Tests -}
testLightingBetween :: Test
testLightingBetween = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      result = lighting m light pos eyev normalv
  assertEqual "between" (Color 1.9 1.9 1.9) result

testLightingBetween45 :: Test
testLightingBetween45 = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 (sqrt 2 / 2) (- sqrt 2 / 2)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      result = lighting m light pos eyev normalv
  assertEqual "between 45" (Color 1.0 1.0 1.0) result

testLightingOpposite45 :: Test
testLightingOpposite45 = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
      result = lighting m light pos eyev normalv
  assertEqual "opposite 45" (Color 0.7364 0.7364 0.7364) result

testLightingInPath :: Test
testLightingInPath = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 (- sqrt 2 / 2) (- sqrt 2 / 2)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
      result = lighting m light pos eyev normalv
  assertEqual "in path" (Color 1.6364 1.6364 1.6364) result

testLightingBehind :: Test
testLightingBehind = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 10) (Color 1 1 1)
      result = lighting m light pos eyev normalv
  assertEqual "behind" (Color 0.1 0.1 0.1) result

tests :: Test
tests =
  TestList
    [ testLightingBetween,
      testLightingBetween45,
      testLightingOpposite45,
      testLightingInPath,
      testLightingBehind
    ]