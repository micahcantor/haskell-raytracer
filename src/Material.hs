module Material where

import Color (cAdd, cMult, cSub)
import Transformation (identity, inverse, mpMult)
import Types (Color (..), Material (Material), Pattern (..), Point (..), Shape (..))

defaultMaterial :: Material
defaultMaterial = Material (Color 1 1 1) 0.1 0.9 0.9 200 0.0 1.0 0 defaultPattern

defaultPattern :: Pattern
defaultPattern = Pattern [] identity (\_ (Point x y z) -> Color x y z)

black, white :: Color
black = Color 0 0 0
white = Color 1 1 1

patternAtShape :: Pattern -> Shape -> Point -> Color
patternAtShape p@(Pattern _ patternTransform colorAt) (Shape _ _ _ shapeTransform) worldPoint =
  let objectPoint = inverse shapeTransform `mpMult` worldPoint
      patternPoint = inverse patternTransform `mpMult` objectPoint
   in colorAt p patternPoint

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
  let distance = c2 `cSub` c1
      fraction = x - fromInteger (floor x)
   in c1 `cAdd` (fraction `cMult` distance)

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

{- A pattern used in some tests -}
testPattern :: Pattern
testPattern = defaultPattern {colorAt = \_ (Point x y z) -> Color x y z}