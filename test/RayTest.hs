module RayTest (tests) where

import Types (Ray(..), Point(..), Vec(..))
import Ray (position)
import Test.HUnit (Test (..), assertEqual)

testPosition :: Test
testPosition = TestCase $ do
  let r = Ray (Point 2 3 4) (Vec 1 0 0)
  assertEqual "position1" (Point 2 3 4) (position r 0)
  assertEqual "position2" (Point 3 3 4) (position r 1)

tests :: Test
tests = TestList [testPosition]