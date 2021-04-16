module Material where

import Color (Color (..))
import VecPoint (Point(..))
import Shape

data Material = Material
  { color :: Color,
    ambient :: Float,
    diffuse :: Float,
    specular :: Float,
    shininess :: Float,
    pattern :: Pattern
  }
  deriving (Show, Eq)

defaultMaterial :: Material
defaultMaterial = Material (Color 1 1 1) 0.1 0.9 0.9 200 emptyPattern

type Pattern = [Color]

emptyPattern :: Pattern
emptyPattern = []

black, white :: Color
black = Color 0 0 0
white = Color 1 1 1

stripePattern :: Color -> Color -> Pattern
stripePattern c1 c2 = [c1, c2]

stripeAt :: Pattern -> Point -> Color
stripeAt (c1:c2:_) (Point x _ _)
  | even (floor x) = c1
  | otherwise = c2

stripeAtShape :: Pattern -> Shape -> Point -> Color