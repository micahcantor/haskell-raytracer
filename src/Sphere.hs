module Sphere where

import Data.Matrix (transpose)
import Material (Material, defaultMaterial)
import Ray (Ray (..))
import Transformation (Transformation, inverse, mpMult, mvMult, translation, identity)
import VecPoint (Point (..), Vec (..), normalize, pSub)

data Sphere = Sphere
  { center :: Point,
    radius :: Float,
    transformation :: Transformation,
    material :: Material
  }
  deriving (Show, Eq)

unitSphere :: Sphere
unitSphere = Sphere (Point 0 0 0) 1 identity defaultMaterial

normalAt :: Sphere -> Point -> Vec
normalAt (Sphere center _ t _) p =
  let objectPoint = inverse t `mpMult` p
      objectNormal = objectPoint `pSub` center
      worldNormal = transpose (inverse t) `mvMult` objectNormal
   in normalize worldNormal
