module LightTest (tests) where

import Constants ( defaultSphere, defaultMaterial, black, white )
import Light (areaLight, lighting, pointOnLight)
import Material (stripePattern)
import Test.HUnit (Test (..), assertEqual, runTestTT)
import Types
  ( Color (Color),
    Light (..),
    Material (..),
    Point (Point),
    Shape (..),
    Vec (Vec),
    World (..),
  )
import VecPoint (normalize, pSub)
import World (defaultWorld, intensityAt)

testLightingBetween :: Test
testLightingBetween = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      result = lighting m defaultSphere light pos eyev normalv 1.0
  assertEqual "between" (Color 1.9 1.9 1.9) result

testLightingBetween45 :: Test
testLightingBetween45 = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 (sqrt 2 / 2) (- sqrt 2 / 2)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      result = lighting m defaultSphere light pos eyev normalv 1.0
  assertEqual "between 45" (Color 1.0 1.0 1.0) result

testLightingOpposite45 :: Test
testLightingOpposite45 = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
      result = lighting m defaultSphere light pos eyev normalv 1.0
  assertEqual "opposite 45" (Color 0.7364 0.7364 0.7364) result

testLightingInPath :: Test
testLightingInPath = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 (- sqrt 2 / 2) (- sqrt 2 / 2)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 10 (-10)) (Color 1 1 1)
      result = lighting m defaultSphere light pos eyev normalv 1.0
  assertEqual "in path" (Color 1.6364 1.6364 1.6364) result

testLightingBehind :: Test
testLightingBehind = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 10) (Color 1 1 1)
      result = lighting m defaultSphere light pos eyev normalv 1.0
  assertEqual "behind" (Color 0.1 0.1 0.1) result

testLightingInShadow :: Test
testLightingInShadow = TestCase $ do
  let m = defaultMaterial
      pos = Point 0 0 0
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) (Color 1 1 1)
      result = lighting m defaultSphere light pos eyev normalv 0.0
  assertEqual "between when in shadow" (Color 0.1 0.1 0.1) result

testLightingPattern :: Test
testLightingPattern = TestCase $ do
  let m = defaultMaterial {ambient = 1, diffuse = 0, specular = 0, pattern = Just (stripePattern white black)}
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      light = PointLight (Point 0 0 (-10)) white
      c1 = lighting m defaultSphere light (Point 0.9 0 0) eyev normalv 1.0
      c2 = lighting m defaultSphere light (Point 1.1 0 0) eyev normalv 1.0
  assertEqual "first stripe is white" white c1
  assertEqual "second stripe is black" black c2

testLightingIntensity :: Test
testLightingIntensity = TestCase $ do
  let [s1, s2] = objects defaultWorld
      mat = defaultMaterial {ambient = 0.1, diffuse = 0.9, specular = 0, color = white}
      shape = s1 {material = mat}
      w = defaultWorld {lights = [PointLight (Point 0 0 (-10)) white], objects = [shape, s2]}
      light = head (lights w)
      p = Point 0 0 (-1)
      eyev = Vec 0 0 (-1)
      normalv = Vec 0 0 (-1)
      intensities = [1, 0.5, 0]
      results = [white, Color 0.55 0.55 0.55, Color 0.1 0.1 0.1]
      colors = map (lighting mat shape light p eyev normalv) intensities
  assertEqual "lighting intensity is correct" results colors

testCreateAreaLight :: Test
testCreateAreaLight = TestCase $ do
  let corner = Point 0 0 0
      v1 = Vec 2 0 0
      v2 = Vec 0 0 1
      light = areaLight corner (v1, 4) (v2, 2) False white
      result = AreaLight corner (Vec 0.5 0 0) (Vec 0 0 0.5) 4 2 8 False white
  assertEqual "create area light" light result

testPointOnLight :: Test
-- tests point on light without random jittering
testPointOnLight = TestCase $ do
  let corner = Point 0 0 0
      v1 = Vec 2 0 0
      v2 = Vec 0 0 1
      light = areaLight corner (v1, 4) (v2, 2) False white
      us = [0, 1, 0, 2, 3]
      vs = [0, 0, 1, 0, 1]
      results =
        [ Point 0.25 0 0.25,
          Point 0.75 0 0.25,
          Point 0.25 0 0.75,
          Point 1.25 0 0.25,
          Point 1.75 0 0.75
        ]
      points = zipWith (pointOnLight light) us vs
  assertEqual "point on light" results points

testLightingAreaLight :: Test
testLightingAreaLight = TestCase $ do
  let corner = Point (-0.5) (-0.5) (-5)
      v1 = Vec 1 0 0
      v2 = Vec 0 1 0
      light = areaLight corner (v1, 2) (v2, 2) False white
      shape =
        defaultSphere
          { material =
              defaultMaterial
                { ambient = 0.1,
                  diffuse = 0.9,
                  specular = 0,
                  color = white
                }
          }
      eye = Point 0 0 (-5)
      points = [Point 0 0 (-1), Point 0 0.7071 (-0.7071)]
      results = [Color 0.9965 0.9965 0.9965, Color 0.6232 0.6232 0.6232]
      eyevs = map (\pt -> normalize (eye `pSub` pt)) points
      normalvs = map (\(Point x y z) -> Vec x y z) points
      applyLighting pt eyev normalv = lighting (material shape) shape light pt eyev normalv 1.0
      lightings = zipWith3 applyLighting points eyevs normalvs
  assertEqual "lighting with an area light works" results lightings

tests :: Test
tests =
  TestList
    [ testLightingBetween,
      testLightingBetween45,
      testLightingOpposite45,
      testLightingInPath,
      testLightingBehind,
      testLightingInShadow,
      testLightingPattern,
      testLightingIntensity,
      testCreateAreaLight,
      testPointOnLight,
      testLightingAreaLight
    ]