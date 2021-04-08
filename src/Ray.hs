module Ray where

import Transformation (Transformation, mpMult, mvMult)
import VecPoint (Point (..), Vec (..), vMult, vpAdd)

data Ray = Ray Point Vec -- origin, direction
  deriving (Show, Eq)

position :: Ray -> Float -> Point
position (Ray origin direction) t = origin `vpAdd` (t `vMult` direction)

transform :: Ray -> Transformation -> Ray
transform (Ray p v) m = Ray (m `mpMult` p) (m `mvMult` v)
