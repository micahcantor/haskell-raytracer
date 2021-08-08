module TransformationTest (tests) where

import qualified Data.Matrix as Matrix (fromLists, toList)
import Test.HUnit (Assertable (assert), Test (..), assertEqual, assertBool)
import Transformation
  ( identity,
    inverse,
    mpMult,
    mvMult,
    rotationX,
    rotationY,
    rotationZ,
    scaling,
    shearing,
    translation,
    viewTransform,
  )
import Types (Point (Point), Vec (Vec), (~=))

testMultiplyByTranslation :: Test
testMultiplyByTranslation = TestCase $ do
  let transform = translation 5 (-3) 2
      p = Point (-3) 4 5
  assertEqual "multiply by translation" (Point 2 1 7) (transform `mpMult` p)

testMultiplyByInverseTranslation :: Test
testMultiplyByInverseTranslation = TestCase $ do
  let transform = inverse (translation 5 (-3) 2)
      p = Point (-3) 4 5
  assertEqual "multiply by translation" (Point (-8) 7 3) (transform `mpMult` p)

testScalingPoint :: Test
testScalingPoint = TestCase $ do
  let transform = scaling 2 3 4
      p = Point (-4) 6 8
  assertEqual "scaling point" (Point (-8) 18 32) (transform `mpMult` p)

testScalingVec :: Test
testScalingVec = TestCase $ do
  let transform = scaling 2 3 4
      v = Vec (-4) 6 8
  assertEqual "scaling vec" (Vec (-8) 18 32) (transform `mvMult` v)

testInverseScaling :: Test
testInverseScaling = TestCase $ do
  let transform = inverse (scaling 2 3 4)
      v = Vec (-4) 6 8
  assertEqual "inverse of scaling" (Vec (-2) 2 2) (transform `mvMult` v)

testReflectionAsScaling :: Test
testReflectionAsScaling = TestCase $ do
  let transform = scaling (-1) 1 1
      p = Point 2 3 4
  assertEqual "reflection as scaling" (Point (-2) 3 4) (transform `mpMult` p)

testRotationX :: Test
testRotationX = TestCase $ do
  let p = Point 0 1 0
      halfQuarter = rotationX (pi / 4)
      fullQuarter = rotationX (pi / 2)
  assertEqual "half quarter x" (Point 0 (sqrt 2 / 2) (sqrt 2 / 2)) (halfQuarter `mpMult` p)
  assertEqual "full quarter x" (Point 0 0 1) (fullQuarter `mpMult` p)

testInverseRotationX :: Test
testInverseRotationX = TestCase $ do
  let p = Point 0 1 0
      halfQuarter = rotationX (pi / 4)
      inv = inverse halfQuarter
  assertEqual "inverse rotation x" (Point 0 (sqrt 2 / 2) (- sqrt 2 / 2)) (inv `mpMult` p)

testRotationY :: Test
testRotationY = TestCase $ do
  let p = Point 0 0 1
      halfQuarter = rotationY (pi / 4)
      fullQuarter = rotationY (pi / 2)
  assertEqual "half quarter y" (Point (sqrt 2 / 2) 0 (sqrt 2 / 2)) (halfQuarter `mpMult` p)
  assertEqual "full quarter y" (Point 1 0 0) (fullQuarter `mpMult` p)

testRotationZ :: Test
testRotationZ = TestCase $ do
  let p = Point 0 1 0
      halfQuarter = rotationZ (pi / 4)
      fullQuarter = rotationZ (pi / 2)
  assertEqual "half quarter z" (Point (- sqrt 2 / 2) (sqrt 2 / 2) 0) (halfQuarter `mpMult` p)
  assertEqual "full quarter z" (Point (-1) 0 0) (fullQuarter `mpMult` p)

testShearing :: Test
testShearing = TestCase $ do
  let point = Point 2 3 4
      transforms =
        [ shearing 1 0 0 0 0 0,
          shearing 0 1 0 0 0 0,
          shearing 0 0 1 0 0 0,
          shearing 0 0 0 1 0 0,
          shearing 0 0 0 0 1 0,
          shearing 0 0 0 0 0 1
        ]
      results =
        [ Point 5 3 4,
          Point 6 3 4,
          Point 2 5 4,
          Point 2 7 4,
          Point 2 3 6,
          Point 2 3 7
        ]
      shears = map (`mpMult` point) transforms
  assertEqual "shearing points" results shears

testSequencingTransformations :: Test
testSequencingTransformations = TestCase $ do
  let p = Point 1 0 1
      a = rotationX (pi / 2)
      b = scaling 5 5 5
      c = translation 10 5 7
      p2 = a `mpMult` p
      p3 = b `mpMult` p2
      p4 = c `mpMult` p3
  assertEqual "apply rotation first" (Point 1 (-1) 0) p2
  assertEqual "apply scaling second" (Point 5 (-5) 0) p3
  assertEqual "apply translation third" (Point 15 0 7) p4

testChainingTransformations :: Test
testChainingTransformations = TestCase $ do
  let p = Point 1 0 1
      a = rotationX (pi / 2)
      b = scaling 5 5 5
      c = translation 10 5 7
      t = c * b * a
  assertEqual "chaining transformations" (Point 15 0 7) (t `mpMult` p)

testViewTransformDefault :: Test
testViewTransformDefault = TestCase $ do
  let from = Point 0 0 0
      to = Point 0 0 (-1)
      up = Vec 0 1 0
  assertEqual "default" identity (viewTransform from to up)

testViewTransformPositiveZ :: Test
testViewTransformPositiveZ = TestCase $ do
  let from = Point 0 0 0
      to = Point 0 0 1
      up = Vec 0 1 0
  assertEqual "scaling" (scaling (-1) 1 (-1)) (viewTransform from to up)

testViewTransformTranslate :: Test
testViewTransformTranslate = TestCase $ do
  let from = Point 0 0 8
      to = Point 0 0 0
      up = Vec 0 1 0
  assertEqual "translation" (translation 0 0 (-8)) (viewTransform from to up)

testViewTransformArbitrary :: Test
testViewTransformArbitrary = TestCase $ do
  let from = Point 1 3 2
      to = Point 4 (-2) 8
      up = Vec 1 1 0
      t = Matrix.toList (viewTransform from to up)
      result =
          [-0.50709, 0.50709, 0.67612, -2.36643,
            0.76772, 0.60609, 0.12122, -2.82843,
            -0.35857, 0.59761, -0.71714, 0,
            0, 0, 0, 1
          ]
  assertBool "arbitrary view transform" (all (== True) (zipWith (~=) t result))

tests :: Test
tests =
  TestList
    [ testMultiplyByTranslation,
      testMultiplyByInverseTranslation,
      testScalingPoint,
      testScalingVec,
      testInverseScaling,
      testReflectionAsScaling,
      testRotationX,
      testInverseRotationX,
      testRotationY,
      testRotationZ,
      testShearing,
      testSequencingTransformations,
      testChainingTransformations,
      testViewTransformDefault,
      testViewTransformPositiveZ,
      testViewTransformTranslate,
      testViewTransformArbitrary
    ]