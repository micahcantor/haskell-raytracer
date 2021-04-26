module Color where

import Types (Color (..))

toPPM :: Int -> Color -> String
toPPM maxColor (Color r g b) =
  unwords $ map (show . roundTo maxColor . round) [r, g, b]
  where
    roundTo x n = if n > x then x else n

cAdd :: Color -> Color -> Color
cAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

cSub :: Color -> Color -> Color
cSub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)

cMult :: Float -> Color -> Color
cMult c (Color r g b) = Color (c * r) (c * g) (c * b)

hadamard :: Color -> Color -> Color
hadamard (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)