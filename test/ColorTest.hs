module ColorTest where

import Color (scale)
import Test.HUnit (Test (..), assertEqual)
import Types ( Color(..) )

testColorMath :: Test
testColorMath = TestCase $ do
  let c1 = Color 0.9 0.6 0.75
      c2 = Color 0.7 0.1 0.25
      c3 = Color 0.2 0.3 0.4
  assertEqual "add colors" (c1 + c2) (Color 1.6 0.7 1.0)
  assertEqual "sub colors" (c1 - c2) (Color 0.2 0.5 0.5)
  assertEqual "scale color" (2 `scale` c3) (Color 0.4 0.6 0.8)
  assertEqual "multiply colors" (c1 * c2) (Color (0.9 * 0.7) (0.6 * 0.1) (0.75 * 0.25))

tests = TestList [testColorMath]