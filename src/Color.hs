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

scale :: Double -> Color -> Color
scale x (Color r g b) =
  Color (r * x) (g * x) (b * x)