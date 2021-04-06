module Sphere where

import Data.Matrix (identity, transpose)
import Material (Material, defaultMaterial)
import Ray (Ray (..))
import Test.HUnit (Assertion, Test (TestCase, TestLabel, TestList), assertEqual, runTestTT)
import Transformation (Transformation, inverse, mpMult, mvMult, translation)
import VecPoint (Point (..), Vec (..), normalize, pSub)

data Sphere = Sphere
  { center :: Point,
    radius :: Float,
    transformation :: Transformation,
    material :: Material
  }
  deriving (Show, Eq)

unitSphere :: Sphere
unitSphere = Sphere (Point 0 0 0) 1 (identity 4) defaultMaterial

normalAt :: Sphere -> Point -> Vec
normalAt (Sphere center _ t _) p =
  let objectPoint = inverse t `mpMult` p
      objectNormal = objectPoint `pSub` center
      worldNormal = transpose (inverse t) `mvMult` objectNormal
   in normalize worldNormal

{- Tests -}
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
  let s = unitSphere { transformation = translation 0 1 0 }
      n = normalAt s (Point 0 1.70711 (-0.70711))
  assertEqual "equality" (Vec 0 0.70711 (-0.70711)) n
