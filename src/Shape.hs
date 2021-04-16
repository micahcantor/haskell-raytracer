module Shape where

import Data.Matrix (transpose)
import qualified Data.SortedList as SL
import Material (Material, defaultMaterial)
import Ray (Ray (..), position, transform)
import Transformation (Transformation, identity, inverse, mpMult, mvMult, scaling, translation)
import VecPoint (Point (Point), Vec (Vec), dot, epsilon, normalize, pSub, vMult, vNeg, vpAdd)

{- Shape ADT -}
data Shape = Shape
  { localIntersect :: Shape -> Ray -> Intersections,
    localNormalAt :: Point -> Vec,
    material :: Material,
    transform :: Transformation
  }

instance Show Shape where
  show (Shape _ _ m t) = unwords [show m, show t]

instance Eq Shape where
  (Shape _ _ m1 t1) == (Shape _ _ m2 t2) = m1 == m2 && t1 == t2

-- Main shape functions:
intersect :: Shape -> Ray -> Intersections
intersect shape@(Shape localIntersect _ _ transform) ray =
  let localRay = Ray.transform ray (inverse transform)
   in localIntersect shape localRay

normalAt :: Shape -> Point -> Vec
normalAt (Shape _ localNormalAt _ transform) point =
  let inverseTransform = inverse transform
      localPoint = inverseTransform `mpMult` point
      localNormal = localNormalAt localPoint
      worldNormal = transpose inverseTransform `mvMult` localNormal
   in normalize worldNormal

{- Spheres -}
defaultSphere :: Shape
defaultSphere = Shape sphereIntersect sphereNormalAt defaultMaterial identity

sphereIntersect :: Shape -> Ray -> Intersections
sphereIntersect sphere (Ray origin direction)
  | d < 0 = SL.toSortedList []
  | otherwise = SL.toSortedList [t1, t2]
  where
    sphereToRay = origin `pSub` Point 0 0 0 -- sphere is by default centered at origin
    a = direction `dot` direction
    b = 2 * (direction `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1
    d = (b ^ 2) - (4 * a * c)
    t1 = Intersection (((- b) - sqrt d) / (2 * a)) sphere
    t2 = Intersection (((- b) + sqrt d) / (2 * a)) sphere

sphereNormalAt :: Point -> Vec
sphereNormalAt (Point x y z) = Vec x y z

{- Planes -}

defaultPlane :: Shape
defaultPlane = Shape planeIntersect planeNormalAt defaultMaterial identity

planeIntersect :: Shape -> Ray -> Intersections
planeIntersect plane (Ray (Point _ originY _) (Vec _ directionY _)) 
  | abs directionY < epsilon = SL.toSortedList [] -- if y component of vec is zero, parallel to xz plane
  | otherwise = toIntersections [Intersection t plane]
  where
    t = -originY / directionY

planeNormalAt :: Point -> Vec
-- constant normal of the xz-plane, other planes obtained through transformaiton
planeNormalAt _ = Vec 0 1 0 

{- Intersection ADT -}
data Intersection = Intersection Float Shape deriving (Show, Eq) -- t value, intersected object

instance Ord Intersection where
  (Intersection t1 _) <= (Intersection t2 _) = t1 <= t2

type Intersections = SL.SortedList Intersection

headSL :: Intersections -> Intersection
headSL xs = head $ SL.fromSortedList xs

atSL :: Intersections -> Int -> Intersection
xs `atSL` i = head $ SL.fromSortedList $ SL.drop i xs

toIntersections :: [Intersection] -> Intersections
toIntersections = SL.toSortedList

hit :: Intersections -> Maybe Intersection
-- returns the first nonzero intersection, if it exists
hit xs =
  fmap fst $ SL.uncons $ SL.filter (\(Intersection t _) -> t > 0) xs

{- Computation -}
-- a structure to hold: inside, t, object, point, eye, normal, over-point
data Computation = Computation
  { inside :: Bool,
    t :: Float,
    object :: Shape,
    point :: Point,
    eye :: Vec,
    normal :: Vec,
    over :: Point
  }
  deriving (Eq)

prepareComputation :: Ray -> Intersection -> Computation
-- precomputes the state of an intersection
prepareComputation r@(Ray origin direction) (Intersection t object) =
  let point = position r t
      eyev = vNeg direction
      normalv = normalAt object point
      normalDotEye = normalv `dot` eyev
      newNormalv
        | normalDotEye < 0 = vNeg normalv
        | otherwise = normalv
      inside = normalDotEye < 0
      overPoint = point `vpAdd` ((200 * epsilon) `vMult` newNormalv)
   in Computation inside t object point eyev newNormalv overPoint