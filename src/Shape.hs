module Shape where

import Data.Matrix (transpose)
import qualified Data.SortedList as SL
import Material (Material, defaultMaterial)
import Ray (Ray (..), position, transform)
import Transformation (Transformation, identity, inverse, mpMult, mvMult, scaling, translation)
import VecPoint (Point (Point), Vec (Vec), dot, normalize, pSub, vNeg, vMult, epsilon, vpAdd)

{- Shape ADT -}
data Shape
  = Sphere {spMaterial :: Material, spTransform :: Transformation}
  | Plane {plMaterial :: Material, plTransfrom :: Transformation}
  deriving (Show, Eq)

-- Main shape functions:
intersect :: Shape -> Ray -> Intersections
intersect s@(Sphere material transform) ray 
  | d < 0 = SL.toSortedList []
  | otherwise = SL.toSortedList [t1, t2]
  where
    (Ray origin direction) = Ray.transform ray (inverse transform)
    sphereToRay = origin `pSub` Point 0 0 0 -- sphere is by default centered at origin
    a = direction `dot` direction
    b = 2 * (direction `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1
    d = (b ^ 2) - (4 * a * c)
    t1 = Intersection (((- b) - sqrt d) / (2 * a)) s
    t2 = Intersection (((- b) + sqrt d) / (2 * a)) s
intersect (Plane material transform) ray = undefined

normalAt :: Shape -> Point -> Vec
normalAt s@(Sphere material transform) point = 
  let inverseTransform = inverse transform
      objectPoint = inverseTransform `mpMult` point
      objectNormal = objectPoint `pSub` Point 0 0 0
      worldNormal = transpose inverseTransform `mvMult` objectNormal
   in normalize worldNormal
normalAt (Plane material transform) point = undefined 

-- Helpers:
getMaterial :: Shape -> Material
getMaterial (Sphere m _) = m
getMaterial (Plane m _) = m

getTransform :: Shape -> Transformation
getTransform (Sphere _ t) = t
getTransform (Plane _ t) = t

defaultSphere :: Shape
defaultSphere = Sphere defaultMaterial identity

{- Intersection ADT -}
data Intersection = Intersection Float Shape deriving (Show, Eq) -- t value, intersected object

instance Ord Intersection where
  (Intersection t1 _) <= (Intersection t2 _) = t1 <= t2

type Intersections = SL.SortedList Intersection

headSL :: Intersections -> Intersection
headSL xs = head $ SL.fromSortedList xs

atSL :: Intersections -> Int -> Intersection
xs `atSL` i = head $ SL.fromSortedList $ SL.drop i xs

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
      overPoint = point `vpAdd` ((25 * epsilon) `vMult` newNormalv)
   in Computation inside t object point eyev newNormalv overPoint