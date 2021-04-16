module MaterialTest where

import Test.HUnit (Test (..), assertEqual)
import Material ( stripePattern, black, white, stripeAt )
import Types ( Point(Point) )

testStripeAt :: Test
testStripeAt = TestCase $ do
  let p = stripePattern white black
  assertEqual "constant in y (1)" white (stripeAt p (Point 0 0 0))
  assertEqual "constant in y (2)" white (stripeAt p (Point 0 1 0))
  assertEqual "constant in z (1)" white (stripeAt p (Point 0 0 1))
  assertEqual "constant in z (2)" white (stripeAt p (Point 0 0 2))
  assertEqual "alternates in x (1)" white (stripeAt p (Point 0.9 0 0))
  assertEqual "alternates in x (2)" black (stripeAt p (Point 1 0 0))

tests :: Test
tests = TestList [testStripeAt]