module CameraTest where

import Test.HUnit (Test (..), assertEqual)
import Color ( Color(Color) )
import Camera ( Camera(hSize, vSize, transform), defaultCamera, rayForPixel, render )
import Canvas ((!))
import Ray ( Ray(Ray) )
import Transformation ( translation, rotationY, viewTransform )
import VecPoint ( Point(Point), Vec(Vec) )
import World (defaultWorld)

testRayForPixelCenter :: Test
testRayForPixelCenter = TestCase $ do
  let c = defaultCamera {hSize = 201, vSize = 101}
      r = rayForPixel c 100 50
  assertEqual "center" (Ray (Point 0 0 0) (Vec 0 0 (-1))) r

testRayForPixelCorner :: Test
testRayForPixelCorner = TestCase $ do
  let c = defaultCamera {hSize = 201, vSize = 101}
      r = rayForPixel c 0 0
  assertEqual "corner" (Ray (Point 0 0 0) (Vec 0.66519 0.33259 (-0.66519))) r

testRayForPixelTransformed :: Test
testRayForPixelTransformed = TestCase $ do
  let c = defaultCamera {hSize = 201, vSize = 101, transform = rotationY (pi / 4) * translation 0 (-2) 5 }
      r = rayForPixel c 100 50
  assertEqual "transformed" (Ray (Point 0 2 (-5)) (Vec (sqrt 2 / 2) 0 (- sqrt 2 / 2))) r

testRender :: Test
testRender = TestCase $ do
  let w = defaultWorld
      from = Point 0 0 (-5)
      to = Point 0 0 0
      up = Vec 0 1 0
      c = defaultCamera {hSize = 11, vSize = 11, transform = viewTransform from to up}
      image = render c w
  assertEqual "equal color" (Color 0.38066 0.47583 0.2855) (image ! (5, 5))

tests :: Test
tests = TestList [testRayForPixelCenter, testRayForPixelCorner, testRayForPixelTransformed, testRender]