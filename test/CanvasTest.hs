module CanvasTest where

import Test.HUnit (Test (..), assertEqual)
import Canvas ( initCanvas, setPixel, toPPM )
import Types ( Color(Color) )

testToPPM :: Test
testToPPM = TestCase $ do
  let c1 = Color 1.5 0 0
      c2 = Color 0 0.5 0
      c3 = Color (-0.5) 0 1
      canv1 = setPixel c1 0 0 (initCanvas 5 3)
      canv2 = setPixel c2 2 1 canv1
      canv3 = setPixel c3 4 2 canv2
      ppm = toPPM canv3
  assertEqual "canvas to ppm" ppm
    (unlines [
      "P3",
      "5 3",
      "255",
      "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
      "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0",
      "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
    ])

tests = TestList [testToPPM]