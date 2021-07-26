module ShapeTest where

import Types
    ( Shape(..),
      Intersection(Intersection),
      Point(Point),
      Vec(Vec),
      Ray(Ray),
      toIntersections )
import Shape (defaultPlane, defaultSphere, normalAt, intersect)
import Intersection ( headSL )
import Test.HUnit (Test (..), assertEqual)
import Transformation (translation)

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

tests :: Test
tests = TestList [testSphereNormalAt, testSphereNormalAtTranslated, testPlaneIntersectParallel, testPlaneIntersectCoplanar, testPlaneIntersectAbove, testPlaneIntersectBelow]