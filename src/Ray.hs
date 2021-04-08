module Ray where

import VecPoint ( Point(..), Vec(..), vpAdd, vMult )
import Transformation ( Transformation, mpMult, mvMult )
import Test.HUnit (Test (TestCase), assertEqual)

data Ray = Ray Point Vec -- origin, direction
  deriving (Show, Eq)

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `vpAdd` (t `vMult` direction)

transform :: Ray -> Transformation -> Ray
transform (Ray p v) m = Ray (m `mpMult` p) (m `mvMult` v)

{- Tests -}
testPosition :: Test
testPosition = TestCase $ do
  let r = Ray (Point 2 3 4) (Vec 1 0 0)
  assertEqual "position1" (Point 2 3 4) (position r 0)
  assertEqual "position2" (Point 3 3 4) (position r 1)