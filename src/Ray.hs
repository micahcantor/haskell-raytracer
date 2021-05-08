module Ray where

import Types ( Point, Transformation, Ray(..) )
import Transformation (mpMult, mvMult)
import VecPoint (vMult, vpAdd)

position :: Ray -> Double -> Point
position (Ray origin direction) t = origin `vpAdd` (t `vMult` direction)

transform :: Ray -> Transformation -> Ray
transform (Ray p v) m = Ray (m `mpMult` p) (m `mvMult` v)
