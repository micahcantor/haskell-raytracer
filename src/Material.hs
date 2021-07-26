module Material where

import Transformation (identity, inverse, mpMult)
import Types (Color (..), Material (..), Pattern (..), Point (..), Shape (..), getTransformation)
import Color (scale)

defaultMaterial :: Material
defaultMaterial =
  Material
    { color = Color 1 1 1,
      ambient = 0.1,
      diffuse = 0.9,
      specular = 0.9,
      shininess = 200,
      reflective = 0.0,
      refractive = 1.0,
      transparency = 0,
      pattern = defaultPattern
    }

glass :: Material
glass = defaultMaterial {transparency = 1.0, refractive = 1.5}

defaultPattern :: Pattern
defaultPattern = Pattern [] identity (\_ (Point x y z) -> Color x y z)

black, white :: Color
black = Color 0 0 0
white = Color 1 1 1

patternAtShape :: Pattern -> Shape -> Point -> Color
patternAtShape p@(Pattern _ patternTransform colorAt) shape worldPoint =
  colorAt p patternPoint
  where
    shapeTransform = getTransformation shape
    objectPoint = inverse shapeTransform `mpMult` worldPoint
    patternPoint = inverse patternTransform `mpMult` objectPoint

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

{- A pattern used in some tests -}
testPattern :: Pattern
testPattern = defaultPattern {colorAt = \_ (Point x y z) -> Color x y z}