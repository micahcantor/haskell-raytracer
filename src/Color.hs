module Color where

import VecPoint (approxEq)

data Color = Color Float Float Float deriving (Show)

toPPM :: Int -> Color -> String
toPPM maxColor (Color r g b) =
  unwords $ map (show . roundTo maxColor . round) [r, g, b]
  where
    roundTo x n = if n > x then x else n

instance Eq Color where
  (Color r1 g1 b1) == (Color r2 g2 b2) =
    approxEq r1 r2 && approxEq g1 g2 && approxEq b1 b2

cAdd :: Color -> Color -> Color
cAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (b1 + b2) (g1 + g2)

cSub :: Color -> Color -> Color
cSub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (b1 - b2) (g1 - g2)

cMult :: Float -> Color -> Color
cMult c (Color r g b) = Color (c * r) (c * g) (c * b)

hadamard :: Color -> Color -> Color
hadamard (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)