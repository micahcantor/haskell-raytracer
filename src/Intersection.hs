module Intersection where

import Data.SortedList as SL
  ( drop,
    filter,
    fromSortedList,
    toSortedList,
    uncons,
  )
import Ray (position)
import Shape (normalAt)
import Types
  ( Computation (Computation),
    Intersection (..),
    Intersections,
    Ray (..),
  )
import VecPoint (dot, reflect, epsilon, vMult, vNeg, vpAdd)

headSL :: Intersections -> Intersection
headSL xs = head $ SL.fromSortedList xs

atSL :: Intersections -> Int -> Intersection
xs `atSL` i = head $ SL.fromSortedList $ SL.drop i xs

hit :: Intersections -> Maybe Intersection
-- returns the first nonzero intersection, if it exists
hit xs =
  fmap fst $ SL.uncons $ SL.filter (\(Intersection t _) -> t > 0) xs

{- Computation -}
prepareComputation :: Ray -> Intersection -> Computation
-- precomputes the state of an intersection
prepareComputation r@(Ray origin direction) (Intersection t object) =
  let point = Ray.position r t
      eyev = vNeg direction
      normalv = normalAt object point
      normalDotEye = normalv `dot` eyev
      newNormalv
        | normalDotEye < 0 = vNeg normalv
        | otherwise = normalv
      reflectv = reflect direction newNormalv
      inside = normalDotEye < 0
      overPoint = point `vpAdd` ((200 * epsilon) `vMult` newNormalv)
   in Computation inside t object point eyev newNormalv reflectv overPoint