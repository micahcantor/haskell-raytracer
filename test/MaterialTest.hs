module MaterialTest where

import Material (black, checkerPattern, defaultPattern, gradientPattern, patternAtShape, ringPattern, stripeAt, stripePattern, white, testPattern)
import Shape (defaultSphere)
import Test.HUnit (Test (..), assertEqual)
import Transformation (scaling, translation)
import Types (Color (..), Pattern (..), Point (Point), Shape (..))

testStripeAt :: Test
testStripeAt = TestCase $ do
  let p = stripePattern white black
  assertEqual "constant in y (1)" white (stripeAt p (Point 0 0 0))
  assertEqual "constant in y (2)" white (stripeAt p (Point 0 1 0))
  assertEqual "constant in z (1)" white (stripeAt p (Point 0 0 1))
  assertEqual "constant in z (2)" white (stripeAt p (Point 0 0 2))
  assertEqual "alternates in x (1)" white (stripeAt p (Point 0.9 0 0))
  assertEqual "alternates in x (2)" black (stripeAt p (Point 1 0 0))
  assertEqual "alternates in x (3)" white (stripeAt p (Point 2.5 0 0))

testGradientAt :: Test
testGradientAt = TestCase $ do
  let p@(Pattern _ _ colorAt) = gradientPattern white black
  assertEqual "starts at white" white (colorAt p (Point 0 0 0))
  assertEqual "gradient 2" (Color 0.75 0.75 0.75) (colorAt p (Point 0.25 0 0))
  assertEqual "gradient 3" (Color 0.5 0.5 0.5) (colorAt p (Point 0.5 0 0))
  assertEqual "gradient 4" (Color 0.25 0.25 0.25) (colorAt p (Point 0.75 0 0))

testRingAt :: Test
testRingAt = TestCase $ do
  let p@(Pattern _ _ colorAt) = ringPattern white black
  assertEqual "ring center" white (colorAt p (Point 0 0 0))
  assertEqual "ring 2" black (colorAt p (Point 1 0 0))
  assertEqual "ring 3" black (colorAt p (Point 0 0 1))
  assertEqual "ring 4" black (colorAt p (Point 0.708 0 0.708))

testCheckerAt :: Test
testCheckerAt = TestCase $ do
  let p@(Pattern _ _ colorAt) = checkerPattern white black
  assertEqual "repeat in x 1" white (colorAt p (Point 0 0 0))
  assertEqual "repeat in x 2" white (colorAt p (Point 0.99 0 0))
  assertEqual "repeat in y 1" white (colorAt p (Point 0 0.99 0))
  assertEqual "repeat in y 2" black (colorAt p (Point 0 1.01 0))
  assertEqual "repeat in z 1" white (colorAt p (Point 0 0 0.99))
  assertEqual "repeat in z 2" black (colorAt p (Point 0 0 1.01))

testPatternAtShape :: Test
testPatternAtShape = TestCase $ do
  let sphere = defaultSphere {transform = scaling 2 2 2}
      pat = testPattern
   in assertEqual "pattern with shape transform" (Color 1 1.5 2) (patternAtShape pat sphere (Point 2 3 4))

testPatternAtShapePatternTransform :: Test
testPatternAtShapePatternTransform = TestCase $ do
  let sphere = defaultSphere
      pat = testPattern {patTransform = scaling 2 2 2}
   in assertEqual "pattern with pattern transform" (Color 1 1.5 2) (patternAtShape pat sphere (Point 2 3 4))

testPatternAtBothTransform :: Test
testPatternAtBothTransform = TestCase $ do
  let sphere = defaultSphere {transform = scaling 2 2 2}
      pat = testPattern {patTransform = translation 0.5 1 1.5}
   in assertEqual "pattern with shape and pattern transform" (Color 0.75 0.5 0.25) (patternAtShape pat sphere (Point 2.5 3 3.5))

tests :: Test
tests =
  TestList
    [ testStripeAt,
      testGradientAt,
      testRingAt,
      testCheckerAt,
      testPatternAtShape,
      testPatternAtShapePatternTransform,
      testPatternAtBothTransform
    ]