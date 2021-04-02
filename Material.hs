module Material where

data Material = Material
  { ambient :: Float,
    diffuse :: Float,
    specular :: Float,
    shininess :: Float
  }
  deriving (Show, Eq)

defaultMaterial :: Material
defaultMaterial = Material 0.1 0.9 0.9 200