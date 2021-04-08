module Material where

import Color ( Color(..) )

data Material = Material
  { color :: Color,
    ambient :: Float,
    diffuse :: Float,
    specular :: Float,
    shininess :: Float
  }
  deriving (Show, Eq)

defaultMaterial :: Material
defaultMaterial = Material (Color 1 1 1) 0.1 0.9 0.9 200