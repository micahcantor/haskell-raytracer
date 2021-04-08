module Transformation where

import Canvas (Canvas)
import Color (Color)
import Data.Matrix (Matrix, detLU, diagonalList, fromList, identity, setElem, toList)
import qualified Data.Matrix (inverse)
import Test.HUnit (Test (TestCase, TestList), assertEqual, runTestTT)
import VecPoint (Point (..), Vec (..), cross, normalize, pSub)

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

{- View Transformations -}
viewTransform :: Point -> Point -> Vec -> Transformation
viewTransform from@(Point fromX fromY fromZ) to up =
  let forward@(Vec forwardX forwardY forwardZ) = normalize (to `pSub` from)
      left@(Vec leftX leftY leftZ) = forward `cross` up
      trueUp@(Vec trueUpX trueUpY trueUpZ) = left `cross` forward
      values =
        [ (leftX, (1, 1)),
          (leftY, (1, 2)),
          (leftZ, (1, 3)),
          (trueUpX, (2, 1)),
          (trueUpY, (2, 2)),
          (trueUpX, (2, 3)),
          (- forwardX, (3, 1)),
          (- forwardY, (3, 2)),
          (- forwardZ, (3, 3))
        ]
      orientation = setElems values (identity 4)
   in orientation * translation (- fromX) (- fromY) (- fromZ)

{- Tests -}
testViewTransformDefault :: Test
testViewTransformDefault = TestCase $ do
  let from = Point 0 0 0
      to = Point 0 0 (-1)
      up = Vec 0 1 0
  assertEqual "default" (identity 4) (viewTransform from to up)

testViewTransformPositiveZ :: Test
testViewTransformPositiveZ = TestCase $ do
  let from = Point 0 0 0
      to = Point 0 0 1
      up = Vec 0 1 0
  assertEqual "scaling" (scaling (-1) 1 (-1)) (viewTransform from to up)

testViewTransformTranslate :: Test
testViewTransformTranslate = TestCase $ do
  let from = Point 0 0 8
      to = Point 0 0 0
      up = Vec 0 1 0
  assertEqual "translation" (translation 0 0 (-8)) (viewTransform from to up)

tests :: Test
tests = TestList [testViewTransformDefault, testViewTransformPositiveZ, testViewTransformTranslate]