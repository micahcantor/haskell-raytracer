module RayTest (tests) where

import Test.HUnit (Test(..), assertEqual)
import VecPoint ( Point(Point), Vec(Vec) )
import Ray ( position, Ray(Ray) )

testPosition :: Test
testPosition = TestCase $ do
  let r = Ray (Point 2 3 4) (Vec 1 0 0)
  assertEqual "position1" (Point 2 3 4) (position r 0)
  assertEqual "position2" (Point 3 3 4) (position r 1)

tests :: Test
tests = TestList [testPosition]