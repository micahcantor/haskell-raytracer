{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types where

{- Declares types for export to avoid cyclic module annoyances -}

import Data.Matrix (Matrix)
import Data.SortedList as SL (SortedList, toSortedList)

(~=) :: Double -> Double -> Bool
a ~= b = abs (a - b) < 0.0001

(~/=) :: Double -> Double -> Bool
a ~/= b = not (a ~= b)

{- VECPOINT -}
data Vec = Vec Double Double Double deriving (Show)

data Point = Point Double Double Double deriving (Show)

-- for testing convenience
instance Eq Vec where
  (Vec x1 y1 z1) == (Vec x2 y2 z2) =
    x1 ~= x2 && y1 ~= y2 && z1 ~= z2

instance Eq Point where
  (Point x1 y1 z1) == (Point x2 y2 z2) =
    x1 ~= x2 && y1 ~= y2 && z1 ~= z2

{- RAY -}
data Ray = Ray Point Vec -- origin, direction
  deriving (Show, Eq)

{- COLOR -}
data Color = Color Double Double Double deriving (Show)

instance Eq Color where
  (Color r1 g1 b1) == (Color r2 g2 b2) =
    r1 ~= r2 && g1 ~= g2 && b1 ~= b2

instance Num Color where
  (Color r1 g1 b1) + (Color r2 g2 b2) =
    Color (r1 + r2) (g1 + g2) (b1 + b2)
  negate (Color r g b) =
    Color (- r) (- g) (- b)
  (Color r1 g1 b1) * (Color r2 g2 b2) =
    Color (r1 * r2) (g1 * g2) (b1 * b2)
  abs (Color r g b) =
    Color (abs r) (abs g) (abs b)
  signum (Color r g b) =
    Color (signum r) (signum g) (signum b)
  fromInteger int =
    Color (fromInteger int) (fromInteger int) (fromInteger int)

{- CANVAS -}
type Canvas = Matrix Color

{- CAMERA -}
data Camera = Camera
  { hSize :: Int,
    vSize :: Int,
    fov :: Double,
    camTransform :: Transformation
  }

{- SHAPES -}
data Shape
  = Sphere
      { material :: Material,
        transform :: Transformation,
        parent :: Maybe Shape
      }
  | Plane
      { material :: Material,
        transform :: Transformation,
        parent :: Maybe Shape
      }
  | Cube
      { material :: Material,
        transform :: Transformation,
        parent :: Maybe Shape
      }
  | Cylinder
      { material :: Material,
        transform :: Transformation,
        parent :: Maybe Shape,
        minY :: Double,
        maxY :: Double,
        closed :: Bool
      }
  | Cone
      { material :: Material,
        transform :: Transformation,
        parent :: Maybe Shape,
        minY :: Double,
        maxY :: Double,
        closed :: Bool
      }
  | Group
      { transform :: Transformation,
        parent :: Maybe Shape,
        children :: [Shape]
      }
  deriving (Show, Eq)

getMaterial :: Shape -> Material
getMaterial Sphere {material} = material
getMaterial Plane {material} = material
getMaterial Cube {material} = material
getMaterial Cylinder {material} = material
getMaterial Cone {material} = material

getTransformation :: Shape -> Transformation
getTransformation Sphere {transform} = transform
getTransformation Plane {transform} = transform
getTransformation Cube {transform} = transform
getTransformation Cylinder {transform} = transform
getTransformation Cone {transform} = transform

{- Intersection -}
data Intersection = Intersection Double Shape deriving (Show, Eq) -- t value, intersected object

instance Ord Intersection where
  (Intersection t1 _) <= (Intersection t2 _) = t1 <= t2

type Intersections = SL.SortedList Intersection

toIntersections :: [Intersection] -> Intersections
toIntersections = SL.toSortedList

data Computation = Computation
  { inside :: Bool,
    t :: Double,
    object :: Shape,
    point :: Point,
    eye :: Vec,
    normal :: Vec,
    reflect :: Vec,
    over :: Point,
    under :: Point,
    n1 :: Double,
    n2 :: Double
  }
  deriving (Show, Eq)

{- MATERIAL -}
data Material = Material
  { color :: Color,
    ambient :: Double,
    diffuse :: Double,
    specular :: Double,
    shininess :: Double,
    reflective :: Double,
    transparency :: Double,
    refractive :: Double,
    pattern :: Maybe Pattern
  }
  deriving (Show, Eq)

data Pattern = Pattern
  { colors :: [Color],
    patTransform :: Transformation,
    colorAt :: Pattern -> Point -> Color
  }

-- below instances for debugging purposes
instance Show Pattern where
  show (Pattern c t _) = "Pattern " ++ show c ++ show t

instance Eq Pattern where
  (Pattern c1 t1 _) == (Pattern c2 t2 _) = c1 == c2 && t1 == t2

{- LIGHT -}
data Light
  = PointLight
      { position :: Point,
        lightColor :: Color
      }
  | AreaLight
      { corner :: Point,
        uvec :: Vec,
        vvec :: Vec,
        usteps :: Int,
        vsteps :: Int,
        samples :: Int,
        jitter :: Bool,
        lightColor :: Color
      }
  deriving (Show, Eq)

{- TRANSFORMATION -}
type Transformation = Matrix Double

{- WORLD -}
data World = World {lights :: [Light], objects :: [Shape]}
