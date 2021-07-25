module Color where

import Types (Color (..))

toPPM :: Int -> Color -> String
toPPM maxColor (Color r g b) =
  unwords $ map (show . clamp maxColor 0 . round . (* fromIntegral maxColor)) [r, g, b]
  where
    clamp hi lo x
      | x < lo = lo
      | x > hi = hi
      | otherwise = x

cAdd :: Color -> Color -> Color
cAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

cSub :: Color -> Color -> Color
cSub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)

cMult :: Double -> Color -> Color
cMult c (Color r g b) = Color (c * r) (c * g) (c * b)

hadamard :: Color -> Color -> Color
hadamard (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)