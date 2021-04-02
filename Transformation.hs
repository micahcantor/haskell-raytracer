module Transformation where

import VecPoint ( Point(..), Vec(..) )
import Color ( Color )
import Canvas ( Canvas )
import Data.Matrix (Matrix, detLU, diagonalList, fromList, identity, setElem, toList)
import qualified Data.Matrix (inverse)

type Transformation = Matrix Float

{- Helper functions -}
setElems :: [(a, (Int, Int))] -> Matrix a -> Matrix a
setElems [] m = m
setElems ((el, (row, col)) : xs) m =
  setElems xs (setElem el (row, col) m)

setPixels :: Color -> [(Int, Int)] -> Canvas -> Canvas
setPixels co pairs ca =
  let newPairs = map (\(x, y) -> (co, (x, y))) pairs
   in setElems newPairs ca

{- Conversions and utility -}
inverse :: Transformation -> Transformation
inverse t = case Data.Matrix.inverse t of
  (Left _) -> error "matrix is not invertible"
  (Right t) -> t

fromPoint :: Point -> Transformation
fromPoint (Point x y z) = fromList 4 1 [x, y, z, 1]

fromVec :: Vec -> Transformation
fromVec (Vec x y z) = fromList 4 1 [x, y, z, 0]

toPoint :: Transformation -> Point
toPoint m =
  let [x, y, z, _] = toList m
   in Point x y z

toVec :: Transformation -> Vec
toVec m =
  let [x, y, z, _] = toList m
   in Vec x y z

mpMult :: Transformation -> Point -> Point
mpMult m p = toPoint (m * fromPoint p)

mvMult :: Transformation -> Vec -> Vec
mvMult m v = toVec (m * fromVec v)

{- Predefined transformations -}
scaling :: Float -> Float -> Float -> Transformation
scaling x y z = diagonalList 4 0 [x, y, z, 1]

translation :: Float -> Float -> Float -> Transformation
translation x y z =
  let values = [(x, (1, 4)), (y, (2, 4)), (z, (3, 4))]
   in setElems values (identity 4)

rotationX :: Float -> Transformation
rotationX r =
  let values = [(cos r, (3, 3)), (- sin r, (2, 3)), (sin r, (3, 2)), (cos r, (2, 2))]
   in setElems values (identity 4)

rotationY :: Float -> Transformation
rotationY r =
  let values = [(cos r, (1, 1)), (- sin r, (3, 1)), (sin r, (1, 3)), (cos r, (3, 3))]
   in setElems values (identity 4)

rotationZ :: Float -> Transformation
rotationZ r =
  let values = [(cos r, (1, 1)), (- sin r, (3, 1)), (sin r, (2, 1)), (cos r, (2, 2))]
   in setElems values (identity 4)

shearing :: Float -> Float -> Float -> Float -> Float -> Float -> Transformation
shearing xy xz yx yz zx zy =
  let values =
        [ (xy, (1, 2)),
          (xz, (1, 3)),
          (yx, (2, 1)),
          (yz, (2, 3)),
          (zx, (3, 1)),
          (zy, (3, 2))
        ]
   in setElems values (identity 4)