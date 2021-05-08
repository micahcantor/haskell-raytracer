module Types where

{- Declares types for export to avoid cyclic module annoyances -}

import Data.Matrix (Matrix)
import Data.SortedList as SL (SortedList, toSortedList)

{- VECPOINT -}
data Vec = Vec Double Double Double deriving (Show)

data Point = Point Double Double Double deriving (Show)

instance Eq Vec where
  (Vec x1 y1 z1) == (Vec x2 y2 z2) =
    approxEq x1 x2 && approxEq y1 y2 && approxEq z1 z2

instance Eq Point where
  (Point x1 y1 z1) == (Point x2 y2 z2) =
    approxEq x1 x2 && approxEq y1 y2 && approxEq z1 z2

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 0.0001

{- END VECPOINT -}

{- RAY -}
data Ray = Ray Point Vec -- origin, direction
  deriving (Show, Eq)

{- END RAY -}

{- COLOR -}
data Color = Color Double Double Double deriving (Show)

instance Eq Color where
  (Color r1 g1 b1) == (Color r2 g2 b2) =
    approxEq r1 r2 && approxEq g1 g2 && approxEq b1 b2

{- END COLOR -}

{- CANVAS -}
type Canvas = Matrix Color

{- END CANVAS -}

{- CAMERA -}
data Camera = Camera
  { hSize :: Int,
    vSize :: Int,
    fov :: Double,
    camTransform :: Transformation
  }

{- END CAMERA -}

{- SHAPES -}
data Shape = Shape
  { localIntersect :: Shape -> Ray -> Intersections,
    localNormalAt :: Point -> Vec,
    material :: Material,
    transform :: Transformation
  }

instance Show Shape where
  show (Shape _ _ m _) = show m

instance Eq Shape where
  (Shape _ _ m1 t1) == (Shape _ _ m2 t2) = m1 == m2 && t1 == t2

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

{- END SHAPES -}

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
    pattern :: Pattern
  }
  deriving (Show, Eq)

data Pattern = Pattern
  { colors :: [Color],
    patTransform :: Transformation,
    colorAt :: Pattern -> Point -> Color
  }

instance Show Pattern where
  show (Pattern c _ _) = show c

instance Eq Pattern where
  (Pattern c1 t1 _) == (Pattern c2 t2 _) = c1 == c2 && t1 == t2
{- END MATERIAL -}

{- LIGHT -}
data PointLight = PointLight {position :: Point, intensity :: Color}

{- END LIGHT -}

{- TRANSFORMATION -}
type Transformation = Matrix Double

{- END TRANSFORMATION -}

{- WORLD -}
data World = World {lights :: [PointLight], objects :: [Shape]}

{- END WORLD -}
