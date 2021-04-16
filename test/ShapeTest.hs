module ShapeTest where

import Types
    ( Shape(Shape, transform),
      Intersection(Intersection),
      Point(Point),
      Vec(Vec),
      Ray(Ray) )
import Shape (defaultPlane, defaultSphere, headSL, toIntersections, normalAt)
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
  let s = defaultSphere {transform = translation 0 1 0}
      n = normalAt s (Point 0 1.70711 (-0.70711))
  assertEqual "equality" (Vec 0 0.70711 (-0.70711)) n

testPlaneIntersectParallel :: Test
testPlaneIntersectParallel = TestCase $ do
  let p@(Shape localIntersect _ _ _) = defaultPlane
      r = Ray (Point 0 10 0) (Vec 0 0 1)
      xs = localIntersect p r
  assertEqual "when parallel, xs is empty" (toIntersections []) xs

testPlaneIntersectCoplanar :: Test
testPlaneIntersectCoplanar = TestCase $ do
  let p@(Shape localIntersect _ _ _) = defaultPlane
      r = Ray (Point 0 0 0) (Vec 0 0 1)
      xs = localIntersect p r
  assertEqual "when coplanar, xs is empty" (toIntersections []) xs

testPlaneIntersectAbove :: Test
testPlaneIntersectAbove = TestCase $ do
  let p@(Shape localIntersect _ _ _) = defaultPlane
      r = Ray (Point 0 1 0) (Vec 0 (-1) 0)
      xs = localIntersect p r
  assertEqual "above" (Intersection 1 p) (headSL xs)

testPlaneIntersectBelow :: Test
testPlaneIntersectBelow = TestCase $ do
  let p@(Shape localIntersect _ _ _) = defaultPlane
      r = Ray (Point 0 (-1) 0) (Vec 0 1 0)
      xs = localIntersect p r
  assertEqual "below" (Intersection 1 p) (headSL xs)

tests :: Test
tests = TestList [testSphereNormalAt, testSphereNormalAtTranslated, testPlaneIntersectParallel, testPlaneIntersectCoplanar, testPlaneIntersectAbove, testPlaneIntersectBelow]