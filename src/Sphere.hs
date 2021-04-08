module Sphere where

import Data.Matrix (identity, transpose)
import Material (Material, defaultMaterial)
import Ray (Ray (..))
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
