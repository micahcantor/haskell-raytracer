module Material where

import Color (scale)
import Constants (defaultMaterial)
import Shape (worldToObject)
import Transformation (identity, inverse, mpMult)
import Types (Color (..), Material (..), Pattern (..), Point (..), Shape (..))

defaultPattern :: Pattern
defaultPattern = Pattern [] identity (\_ (Point x y z) -> Color x y z)

patternAtShape :: Pattern -> Shape -> Point -> Color
patternAtShape p@(Pattern _ patternTransform colorAt) shape worldPoint =
  colorAt p patternPoint
  where
    shapeTransform = transform shape
    objectPoint = worldToObject shape worldPoint
    patternPoint = inverse patternTransform `mpMult` objectPoint

getMaterial :: Shape -> Material
getMaterial shape
  | material shape == defaultMaterial =
    case parent shape of
      Nothing -> material shape
      Just p -> getMaterial p
  | otherwise = material shape

{- Stripes -}
stripePattern :: Color -> Color -> Pattern
stripePattern c1 c2 = defaultPattern {colors = [c1, c2], colorAt = stripeAt}

stripeAt :: Pattern -> Point -> Color
stripeAt (Pattern (c1 : c2 : _) _ _) (Point x _ _)
  | even (floor x) = c1
  | otherwise = c2

{- Gradients -}
gradientPattern :: Color -> Color -> Pattern
gradientPattern c1 c2 = defaultPattern {colors = [c1, c2], colorAt = gradientAt}

gradientAt :: Pattern -> Point -> Color
gradientAt (Pattern (c1 : c2 : _) _ _) (Point x _ _) =
  let distance = c2 - c1
      fraction = x - fromInteger (floor x)
   in c1 + (fraction `scale` distance)

{- Rings -}
ringPattern :: Color -> Color -> Pattern
ringPattern c1 c2 = defaultPattern {colors = [c1, c2], colorAt = ringAt}

ringAt :: Pattern -> Point -> Color
ringAt (Pattern (c1 : c2 : _) _ _) (Point x _ z)
  | even $ floor $ sqrt ((x ^ 2) + (z ^ 2)) = c1
  | otherwise = c2

{- Checkers -}
checkerPattern :: Color -> Color -> Pattern
checkerPattern c1 c2 = defaultPattern {colors = [c1, c2], colorAt = checkerAt}

checkerAt :: Pattern -> Point -> Color
checkerAt (Pattern (c1 : c2 : _) _ _) (Point x y z)
  | even (floor x + floor y + floor z) = c1
  | otherwise = c2

{- testPattern -}
testPattern :: Pattern
testPattern = Pattern [] identity (\_ (Point x y z) -> Color x y z)