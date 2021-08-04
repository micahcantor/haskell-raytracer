module ShapeTest where

import Intersection (atSL, headSL)
import Shape (defaultCube, defaultPlane, defaultSphere, intersect, normalAt)
import Test.HUnit (Test (..), assertEqual)
import Transformation (translation)
import Types
  ( Intersection (Intersection),
    Point (Point),
    Ray (Ray),
    Shape (..),
    Vec (Vec),
    toIntersections,
  )

testSphereNormalAt :: Test
testSphereNormalAt = TestCase $ do
  let s = defaultSphere
      n1 = normalAt s (Point 1 0 0)
      n2 = normalAt s (Point 0 1 0)
      n3 = normalAt s (Point 0 0 1)
      n4 = normalAt s (Point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
  assertEqual "x axis" (Vec 1 0 0) n1
  assertEqual "y axis" (Vec 0 1 0) n2
  assertEqual "z axis" (Vec 0 0 1) n3

testSphereNormalAtTranslated :: Test
testSphereNormalAtTranslated = TestCase $ do
  let s = defaultSphere {sphereTransform = translation 0 1 0}
      n = normalAt s (Point 0 1.70711 (-0.70711))
  assertEqual "equality" (Vec 0 0.70711 (-0.70711)) n

testPlaneIntersectParallel :: Test
testPlaneIntersectParallel = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 10 0) (Vec 0 0 1)
      xs = p `intersect` r
  assertEqual "when parallel, xs is empty" (toIntersections []) xs

testPlaneIntersectCoplanar :: Test
testPlaneIntersectCoplanar = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      xs = p `intersect` r
  assertEqual "when coplanar, xs is empty" (toIntersections []) xs

testPlaneIntersectAbove :: Test
testPlaneIntersectAbove = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 1 0) (Vec 0 (-1) 0)
      xs = p `intersect` r
  assertEqual "above" (Intersection 1 p) (headSL xs)

testPlaneIntersectBelow :: Test
testPlaneIntersectBelow = TestCase $ do
  let p = defaultPlane
      r = Ray (Point 0 (-1) 0) (Vec 0 1 0)
      xs = p `intersect` r
  assertEqual "below" (Intersection 1 p) (headSL xs)

testCubeIntersectHit :: Test
testCubeIntersectHit = TestCase $ do
  let c = defaultCube
      origins =
        [ Point 5 0.5 0,
          Point (-5) 0.5 0,
          Point 0.5 5 0,
          Point 0.5 (-5) 0,
          Point 0.5 0 5,
          Point 0.5 0 (-5),
          Point 0 0.5 0
        ]
      directions =
        [ Vec (-1) 0 0,
          Vec 1 0 0,
          Vec 0 (-1) 0,
          Vec 0 1 0,
          Vec 0 0 (-1),
          Vec 0 0 1,
          Vec 0 0 1
        ]
      t1s = [4, 4, 4, 4, 4, 4, -1]
      t2s = [6, 6, 6, 6, 6, 6, 1]
      rays = zipWith Ray origins directions
      xs = map (intersect c) rays
      get_t (Intersection t _) = t
  assertEqual "t1 when ray strikes cube" t1s (map (get_t . headSL) xs)
  assertEqual "t2 when ray strikes cube" t2s (map (get_t . (`atSL` 1)) xs)

testCubeIntersectMiss :: Test
testCubeIntersectMiss = TestCase $ do
  let c = defaultCube
      origins =
        [ Point (-2) 0 0,
          Point 0 (-2) 0,
          Point 0 0 (-2),
          Point 2 0 2,
          Point 0 2 2,
          Point 2 2 0
        ]
      directions =
        [ Vec 0.2673 0.5345 0.8018,
          Vec 0.8018 0.2673 0.5345,
          Vec 0.5345 0.8018 0.2673,
          Vec 0 0 (-1),
          Vec 0 (-1) 0,
          Vec (-1) 0 0
        ]
      rays = zipWith Ray origins directions
      xs = map (intersect c) rays
  assertEqual "no intersections when ray misses" (replicate 6 0) (map length xs)

testCubeNormalAt :: Test
testCubeNormalAt = TestCase $ do
  let c = defaultCube
  let points =
        [ Point 1 0.5 (-0.8),
          Point (-1) (-0.2) 0.9,
          Point (-0.4) 1 (-0.1),
          Point 0.3 (-1) (-0.7),
          Point (-0.6) 0.3 1,
          Point 0.4 0.4 (-1),
          Point 1 1 1,
          Point (-1) (-1) (-1)
        ]
  let normals =
        [ Vec 1 0 0,
          Vec (-1) 0 0,
          Vec 0 1 0,
          Vec 0 (-1) 0,
          Vec 0 0 1,
          Vec 0 0 (-1),
          Vec 1 0 0,
          Vec (-1) 0 0
        ]
  assertEqual "normal at various points" normals (map (normalAt c) points)

tests :: Test
tests =
  TestList
    [ testSphereNormalAt,
      testSphereNormalAtTranslated,
      testPlaneIntersectParallel,
      testPlaneIntersectCoplanar,
      testPlaneIntersectAbove,
      testPlaneIntersectBelow,
      testCubeIntersectHit,
      testCubeIntersectMiss,
      testCubeNormalAt
    ]