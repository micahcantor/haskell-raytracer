module WorldTest where

import Data.SortedList as SL (fromSortedList)
import Intersection (atSL, headSL, prepareComputation)
import Material (black, defaultMaterial, defaultPattern, testPattern, white)
import Shape (defaultPlane, defaultSphere)
import Test.HUnit (Test (..), assertEqual, runTestTT)
import Transformation (translation)
import Types
  ( Color (Color),
    Intersection (Intersection),
    Light (..),
    Material (..),
    Pattern,
    Point (Point),
    Ray (Ray),
    Shape (..),
    Vec (Vec),
    World (..),
    toIntersections,
  )
import World
  ( colorAt,
    defaultWorld,
    intensityAt,
    intersect,
    isShadowed,
    maxRecursions,
    reflectedColor,
    refractedColor,
    shadeHit,
  )
import Light (areaLight)

testIntersect :: Test
testIntersect = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      xs = w `intersect` r
  assertEqual "intersecting world with ray" [4, 4.5, 5.5, 6] [t | (Intersection t _) <- SL.fromSortedList xs]

testShadeHit :: Test
testShadeHit = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      shape = head $ objects w
      i = Intersection 4 shape
      comps = prepareComputation r i (toIntersections [i])
  assertEqual "shading a hit" (Color 0.38066 0.47583 0.2855) (shadeHit w comps maxRecursions)

testShadeHitInside :: Test
testShadeHitInside = TestCase $ do
  let w = defaultWorld {lights = [PointLight (Point 0 0.25 0) (Color 1 1 1)]}
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = objects w !! 1
      i = Intersection 0.5 shape
      comps = prepareComputation r i (toIntersections [i])
  assertEqual "shading from inside an object" (Color 0.90498 0.90498 0.90498) (shadeHit w comps maxRecursions)

testShadeHitInShadow :: Test
testShadeHitInShadow = TestCase $ do
  let s1 = defaultSphere
      s2 = defaultSphere {transform = translation 0 0 10}
      w = defaultWorld {lights = [PointLight (Point 0 0 (-10)) (Color 1 1 1)], objects = [s1, s2]}
      r = Ray (Point 0 0 5) (Vec 0 0 1)
      i = Intersection 4 s2
      comps = prepareComputation r i (toIntersections [i])
  assertEqual "shading in shadow" (Color 0.1 0.1 0.1) (shadeHit w comps maxRecursions)

testShadeHitReflective :: Test
testShadeHitReflective = TestCase $ do
  let shape = defaultPlane {material = defaultMaterial {reflective = 0.5}, transform = translation 0 (-1) 0}
      w = defaultWorld {objects = shape : objects defaultWorld}
      r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      i = Intersection (sqrt 2) shape
      comps = prepareComputation r i (toIntersections [i])
  assertEqual "shading reflective material" (Color 0.87677 0.92436 0.82918) (shadeHit w comps maxRecursions)

testColorAtMiss :: Test
testColorAtMiss = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 1 0)
  assertEqual "colorAt miss" (Color 0 0 0) (colorAt w r maxRecursions)

testColorAtHit :: Test
testColorAtHit = TestCase $ do
  let w = defaultWorld
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
  assertEqual "colorAt a hit" (Color 0.38066 0.47583 0.2855) (colorAt w r maxRecursions)

testReflectedColorNonReflective :: Test
testReflectedColorNonReflective = TestCase $ do
  let w@(World _ (s1 : s2 : _)) = defaultWorld
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      shape = s2 {material = defaultMaterial {ambient = 1}}
      i = Intersection 1 shape
      comps = prepareComputation r i (toIntersections [i])
  assertEqual "reflected color for nonreflective" black (reflectedColor w comps maxRecursions)

testReflectedColorReflective :: Test
testReflectedColorReflective = TestCase $ do
  let r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      shape = defaultPlane {material = defaultMaterial {reflective = 0.5}, transform = translation 0 (-1) 0}
      i = Intersection (sqrt 2) shape
      w = defaultWorld {objects = shape : objects defaultWorld}
      comps = prepareComputation r i (toIntersections [i])
  assertEqual "reflected color for reflective surface" (Color 0.19032 0.2379 0.14274) (reflectedColor w comps maxRecursions)

testReflectedColorLimitedRecursion :: Test
testReflectedColorLimitedRecursion = TestCase $ do
  let r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      shape = defaultPlane {material = defaultMaterial {reflective = 0.5}, transform = translation 0 (-1) 0}
      i = Intersection (sqrt 2) shape
      w = defaultWorld {objects = [shape]}
      comps = prepareComputation r i (toIntersections [i])
      remaining = 0
  assertEqual "reflected color with 0 remaining recursions" (Color 0.19032 0.2379 0.14274) (reflectedColor w comps remaining)

testRefractedColorOpaque :: Test
testRefractedColorOpaque = TestCase $ do
  let shape = defaultSphere
      w = defaultWorld {objects = [shape]}
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      xs = toIntersections [Intersection 4 shape, Intersection 6 shape]
      comps = prepareComputation r (headSL xs) xs
  assertEqual "refracted color of opaque surface" (Color 0 0 0) (refractedColor w comps 5)

testRefractedColorMaxRecursion :: Test
testRefractedColorMaxRecursion = TestCase $ do
  let shape = defaultSphere {material = defaultMaterial {transparency = 1.0, refractive = 1.5}}
      w = defaultWorld {objects = [shape]}
      r = Ray (Point 0 0 (-5)) (Vec 0 0 1)
      xs = toIntersections [Intersection 4 shape, Intersection 6 shape]
      comps = prepareComputation r (headSL xs) xs
  assertEqual "refracted color at max recursions" (Color 0 0 0) (refractedColor w comps 0)

testRefractedColorInternalReflection :: Test
testRefractedColorInternalReflection = TestCase $ do
  let shape = defaultSphere {material = defaultMaterial {transparency = 1.0, refractive = 1.5}}
      w = defaultWorld {objects = [shape]}
      r = Ray (Point 0 0 (sqrt 2 / 2)) (Vec 0 1 0)
      xs = toIntersections [Intersection (- sqrt 2 / 2) shape, Intersection (sqrt 2 / 2) shape]
      comps = prepareComputation r (xs `atSL` 1) xs
  assertEqual "refracted color with internal reflection" (Color 0 0 0) (refractedColor w comps 5)

testRefractedColorRefraction :: Test
testRefractedColorRefraction = TestCase $ do
  let [s1, s2] = objects defaultWorld
      a = s1 {material = defaultMaterial {ambient = 1.0, pattern = Just testPattern}}
      b = s2 {material = defaultMaterial {transparency = 1.0, refractive = 1.5}}
      w = defaultWorld {objects = [a, b]}
      r = Ray (Point 0 0 0.1) (Vec 0 1 0)
      xs = toIntersections [Intersection (-0.9899) a, Intersection (-0.4899) b, Intersection 0.4899 b, Intersection 0.9899 a]
      comps = prepareComputation r (xs `atSL` 2) xs
  assertEqual "refracted color on refractive material" (Color 0 0.99888 0.04725) (refractedColor w comps 5)

testShadeHitRefraction :: Test
testShadeHitRefraction = TestCase $ do
  let floor =
        defaultPlane
          { transform = translation 0 (-1) 0,
            material = defaultMaterial {transparency = 0.5, refractive = 1.5}
          }
      ball =
        defaultSphere
          { transform = translation 0 (-3.5) (-0.5),
            material = defaultMaterial {color = Color 1 0 0, ambient = 0.5}
          }
      w = defaultWorld {objects = objects defaultWorld ++ [floor, ball]}
      r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      xs = toIntersections [Intersection (sqrt 2) floor]
      comps = prepareComputation r (headSL xs) xs
  assertEqual "shading transparent material" (Color 0.93642 0.68642 0.68642) (shadeHit w comps 5)

testShadeHitReflectionRefraction :: Test
testShadeHitReflectionRefraction = TestCase $ do
  let floor =
        defaultPlane
          { transform = translation 0 (-1) 0,
            material = defaultMaterial {transparency = 0.5, reflective = 0.5, refractive = 1.5}
          }
      ball =
        defaultSphere
          { transform = translation 0 (-3.5) (-0.5),
            material = defaultMaterial {color = Color 1 0 0, ambient = 0.5}
          }
      w = defaultWorld {objects = objects defaultWorld ++ [floor, ball]}
      r = Ray (Point 0 0 (-3)) (Vec 0 (- sqrt 2 / 2) (sqrt 2 / 2))
      xs = toIntersections [Intersection (sqrt 2) floor]
      comps = prepareComputation r (headSL xs) xs
  assertEqual "shading transparent and refractive material" (Color 0.93391 0.69643 0.69243) (shadeHit w comps 5)

testIsShadowedOcclusion :: Test
testIsShadowedOcclusion = TestCase $ do
  let w = defaultWorld
      lightPos = Point (-10) (-10) (-10)
      points =
        [ Point (-10) (-10) (-10),
          Point 10 10 10,
          Point (-20) (-20) (-20),
          Point (-5) (-5) (-5)
        ]
      shadowed = [False, True, False, False]
  assertEqual "is shadowed test for occlusion" shadowed (map (\p -> isShadowed w p lightPos) points)

testIntensityAt :: Test
testIntensityAt = TestCase $ do
  let w = defaultWorld
      light = head (lights w)
      points =
        [ Point 0 1.0001 0,
          Point (-1.0001) 0 0,
          Point 0 0 (-1.0001),
          Point 0 0 1.0001,
          Point 1.0001 0 0,
          Point 0 (-1.0001) 0,
          Point 0 0 0
        ]
      results =
        [1, 1, 1, 0, 0, 0, 0]
      intensities = map (\p -> intensityAt light p w) points
  assertEqual "intensity at works" results intensities

testAreaLightIntensityAt :: Test 
testAreaLightIntensityAt = TestCase $ do
  let w = defaultWorld 
      corner = Point (-0.5) (-0.5) (-5)
      v1 = Vec 1 0 0
      v2 = Vec 0 1 0
      light = areaLight corner (v1, 2) (v2, 2) False white
      points =
        [ Point 0 0 2,
          Point 1 (-1) 2,
          Point 1.5 0 2,
          Point 1.25 1.25 3,
          Point 0 0 (-2)
        ]
      results = [0, 0.25, 0.5, 0.75, 1]
      intensities = map (\p -> intensityAt light p w) points
  assertEqual "area light intensity at" results intensities

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
      testReflectedColorNonReflective,
      testReflectedColorReflective,
      testRefractedColorMaxRecursion,
      testRefractedColorInternalReflection,
      testRefractedColorRefraction,
      testShadeHitRefraction,
      testShadeHitReflectionRefraction,
      testIsShadowedOcclusion,
      testIntensityAt,
      testAreaLightIntensityAt
    ]