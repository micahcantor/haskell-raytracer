module Constants where

import Transformation ( identity )
import Types
    ( Point(Point),
      Shape(Group, Sphere, Plane, Cube, Cylinder, Cone),
      Color(..),
      Material(..),
      Pattern(Pattern) )

{- Patterns -}
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
      pattern = Nothing
    }

glass :: Material
glass = defaultMaterial {transparency = 1.0, refractive = 1.5}

black, white :: Color
black = Color 0 0 0
white = Color 1 1 1

{- Default shapes -}
defaultSphere :: Shape
defaultSphere = Sphere defaultMaterial identity Nothing

glassSphere :: Shape
glassSphere = Sphere glass identity Nothing

defaultPlane :: Shape
defaultPlane = Plane defaultMaterial identity Nothing

defaultCube :: Shape
defaultCube = Cube defaultMaterial identity Nothing

defaultCylinder :: Shape
defaultCylinder = Cylinder defaultMaterial identity Nothing negInf posInf False

defaultCone :: Shape
defaultCone = Cone defaultMaterial identity Nothing negInf posInf False

defaultGroup :: Shape
defaultGroup = Group identity Nothing []

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0