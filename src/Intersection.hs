module Intersection where

import qualified Data.SortedList as SL
import Ray (Ray (..), position, transform)
import Sphere (Sphere (..), normalAt, unitSphere)
import Transformation (inverse, scaling, translation)
import VecPoint (Point (Point), Vec (Vec), dot, pSub, vNeg)

data Intersection = Intersection Float Sphere -- t value, intersected object
  deriving (Show, Eq)

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
  let zeroIntersection = Intersection 0 unitSphere
   in fmap fst $ SL.uncons $ SL.filterGE zeroIntersection xs

sphereIntersect :: Sphere -> Ray -> Intersections
sphereIntersect s@(Sphere center r t _) ray
  | d < 0 = SL.toSortedList []
  | otherwise = SL.toSortedList [t1, t2]
  where
    (Ray origin direction) = transform ray (inverse t)
    sphereToRay = origin `pSub` center
    a = direction `dot` direction
    b = 2 * (direction `dot` sphereToRay)
    c = (sphereToRay `dot` sphereToRay) - 1
    d = (b ^ 2) - (4 * a * c)
    t1 = Intersection (((- b) - sqrt d) / (2 * a)) s
    t2 = Intersection (((- b) + sqrt d) / (2 * a)) s

data Computation = Computation Bool Float Sphere Point Vec Vec deriving (Show, Eq)
-- inside, t, object, point, eye, normal

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
   in Computation inside t object point eyev newNormalv