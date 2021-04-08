module SphereTest where

import Sphere (Sphere (transformation), normalAt, unitSphere)
import Test.HUnit (Test (..), assertEqual)
import Transformation (translation)
import VecPoint (Point (Point), Vec (Vec))

testNormalAt :: Test
testNormalAt = TestCase $ do
  let s = unitSphere
      n1 = normalAt s (Point 1 0 0)
      n2 = normalAt s (Point 0 1 0)
      n3 = normalAt s (Point 0 0 1)
      n4 = normalAt s (Point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
  assertEqual "x axis" (Vec 1 0 0) n1
  assertEqual "y axis" (Vec 0 1 0) n2
  assertEqual "z axis" (Vec 0 0 1) n3

testNormalAtTranslated :: Test
testNormalAtTranslated = TestCase $ do
  let s = unitSphere {transformation = translation 0 1 0}
      n = normalAt s (Point 0 1.70711 (-0.70711))
  assertEqual "equality" (Vec 0 0.70711 (-0.70711)) n

tests :: Test
tests = TestList [testNormalAt, testNormalAtTranslated]