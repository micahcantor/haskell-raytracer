module Constants where

import Transformation ( identity )
import Types
    ( Point(Point),
      Shape(Group, Sphere, Plane, Cube, Cylinder, Cone, Triangle),
      Color(..),
      Material(..),
      Pattern(Pattern) )
import VecPoint (pSub, normalize, cross)

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

triangle :: Point -> Point -> Point -> Shape
triangle p1 p2 p3 = 
  Triangle defaultMaterial identity Nothing p1 p2 p3 e1 e2 normal
  where
    e1 = p2 `pSub` p1
    e2 = p3 `pSub` p1
    normal = normalize (e2 `cross` e1)

defaultGroup :: Shape
defaultGroup = Group defaultMaterial identity Nothing []

posInf, negInf :: Double
posInf = 1 / 0
negInf = -1 / 0