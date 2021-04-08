module VecPoint where

data Vec = Vec Float Float Float deriving (Show)

data Point = Point Float Float Float deriving (Show)

instance Eq Vec where
  (Vec x1 y1 z1) == (Vec x2 y2 z2) 
    = approxEq x1 x2 && approxEq y1 y2 && approxEq z1 z2

instance Eq Point where
  (Point x1 y1 z1) == (Point x2 y2 z2) 
    = approxEq x1 x2 && approxEq y1 y2 && approxEq z1 z2

approxEq :: Float -> Float -> Bool
approxEq a b = a - b < 0.00001

{- Point functions -}
pAdd :: Point -> Point -> Vec
pAdd (Point x1 y1 z1) (Point x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

pSub :: Point -> Point -> Vec
pSub (Point x1 y1 z1) (Point x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

pMult :: Float -> Point -> Point
pMult c (Point x y z) = Point (c*x) (c*y) (c*z)

pDiv :: Point -> Float -> Point
pDiv (Point x y z) c = Point (x / c) (y / c) (z / c)

{- Basic vector functions -}
vAdd :: Vec -> Vec -> Vec
vAdd (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

vSub :: Vec -> Vec -> Vec
vSub (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

vNeg :: Vec -> Vec
vNeg (Vec x y z) = Vec (-x) (-y) (-z)

vMult :: Float -> Vec -> Vec
vMult c (Vec x y z) = Vec (c*x) (c*y) (c*z)

vDiv :: Vec -> Float -> Vec
vDiv (Vec x y z) c = Vec (x / c) (y / c) (z / c)

{- Point-vector functions -}
vpAdd :: Point -> Vec -> Point
vpAdd (Point x1 y1 z1) (Vec x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

vpSub :: Point -> Vec -> Point
vpSub (Point x1 y1 z1) (Vec x2 y2 z2) = Point (x1 - x2) (y1 - y2) (z1 - z2)

{- Other vector functions -}
magnitude :: Vec -> Float
magnitude (Vec x y z) = sqrt ((x^2) + (y^2) + (z^2))

normalize :: Vec -> Vec
normalize v@(Vec x y z) = Vec (x / mag) (y / mag) (z / mag)
  where mag = magnitude v

dot :: Vec -> Vec -> Float
dot (Vec x1 y1 z1) (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross :: Vec -> Vec -> Vec
cross (Vec x1 y1 z1) (Vec x2 y2 z2) 
  = Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

reflect :: Vec -> Vec -> Vec
reflect vec normal =
  vec `vSub` ((vec `dot` normal) `vMult` (2 `vMult` normal))
