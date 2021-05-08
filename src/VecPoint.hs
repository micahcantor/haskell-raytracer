module VecPoint where

import Types (Point (..), Vec (..))

epsilon :: Double
epsilon = 0.00001

{- Point functions -}
pAdd :: Point -> Point -> Vec
pAdd (Point x1 y1 z1) (Point x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

pSub :: Point -> Point -> Vec
pSub (Point x1 y1 z1) (Point x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

pMult :: Double -> Point -> Point
pMult c (Point x y z) = Point (c * x) (c * y) (c * z)

pDiv :: Point -> Double -> Point
pDiv (Point x y z) c = Point (x / c) (y / c) (z / c)

{- Basic vector functions -}
vAdd :: Vec -> Vec -> Vec
vAdd (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

vSub :: Vec -> Vec -> Vec
vSub (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

vNeg :: Vec -> Vec
vNeg (Vec x y z) = Vec (- x) (- y) (- z)

vMult :: Double -> Vec -> Vec
vMult c (Vec x y z) = Vec (c * x) (c * y) (c * z)

vDiv :: Vec -> Double -> Vec
vDiv (Vec x y z) c = Vec (x / c) (y / c) (z / c)

{- Point-vector functions -}
vpAdd :: Point -> Vec -> Point
vpAdd (Point x1 y1 z1) (Vec x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

vpSub :: Point -> Vec -> Point
vpSub (Point x1 y1 z1) (Vec x2 y2 z2) = Point (x1 - x2) (y1 - y2) (z1 - z2)

{- Other vector functions -}
magnitude :: Vec -> Double
magnitude (Vec x y z) = sqrt ((x ^ 2) + (y ^ 2) + (z ^ 2))

normalize :: Vec -> Vec
normalize v@(Vec x y z) = Vec (x / mag) (y / mag) (z / mag)
  where
    mag = magnitude v

dot :: Vec -> Vec -> Double
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Vec -> Vec -> Vec
cross (Vec x1 y1 z1) (Vec x2 y2 z2) =
  Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

reflect :: Vec -> Vec -> Vec
reflect vec normal =
  vec `vSub` ((vec `dot` normal) `vMult` (2 `vMult` normal))
