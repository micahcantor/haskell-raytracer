module Chapter4 where

import Chapter1 ( Vec (..), Point(..), pMult )
import Chapter2 ( Canvas (..), Color (..), initCanvas, canvasWidth, writeCanvas )
import Data.Matrix ( Matrix, identity, setElem, diagonalList, fromList, toList )

setElems :: [(a, (Int, Int))] -> Matrix a -> Matrix a
setElems [] m = m
setElems ((el, (row, col)) : xs) m =
  setElems xs (setElem el (row, col) m)

setPixels :: Color -> [(Int, Int)] -> Canvas -> Canvas
setPixels co pairs ca = 
  let newPairs = map (\(x, y) -> (co, (x, y))) pairs
   in setElems newPairs ca 

fromPoint :: Point -> Matrix Float 
fromPoint (Point x y z) = fromList 4 1 [x, y, z, 1]

fromVec :: Vec -> Matrix Float
fromVec (Vec x y z) = fromList 4 1 [x, y, z, 0]

toPoint :: Matrix Float -> Point 
toPoint m = 
  let [x, y, z, _] = toList m
   in Point x y z

toVec :: Matrix Float -> Vec
toVec m =
  let [x, y, z, _] = toList m
   in Vec x y z

mpMult :: Matrix Float -> Point -> Point 
mpMult m  p= toPoint (m * fromPoint p)

mvMult :: Matrix Float -> Vec -> Vec
mvMult m v = toVec (m * fromVec v)

scaling :: Float -> Float -> Float -> Matrix Float
scaling x y z = diagonalList 4 0 [x, y, z, 1]

translation :: Float -> Float -> Float -> Matrix Float
translation x y z =
  let values = [(x, (1, 4)), (y, (2, 4)), (z, (3, 4))]
   in setElems values (identity 4)

rotationX :: Float -> Matrix Float
rotationX r =
  let values = [(cos r, (3, 3)), (- sin r, (2, 3)), (sin r, (3, 2)), (cos r, (2, 2))]
   in setElems values (identity 4)

rotationY :: Float -> Matrix Float
rotationY r =
  let values = [(cos r, (1, 1)), (- sin r, (3, 1)), (sin r, (1, 3)), (cos r, (3, 3))]
   in setElems values (identity 4)

rotationZ :: Float -> Matrix Float
rotationZ r =
  let values = [(cos r, (1, 1)), (- sin r, (3, 1)), (sin r, (2, 1)), (cos r, (2, 2))]
   in setElems values (identity 4)

shearing :: Float -> Float -> Float -> Float -> Float -> Float -> Matrix Float
shearing xy xz yx yz zx zy =
  let values = [(xy, (1, 2)), (xz, (1, 3)), (yx, (2, 1)), 
                (yz, (2, 3)), (zx, (3, 1)), (zy, (3, 2))]
   in setElems values (identity 4)

{- Putting it together -}

plotClock :: Canvas -> Canvas
plotClock c =
  let white = Color 255 255 255
      twelveO'Clock = Point 0 0 1
      width = fromIntegral $ canvasWidth c
      rotate n = rotationY (n * (pi / 6))
      toOrigin = translation (width / 2) 0 (width / 2)
      radiusScale = scaling (3/8 * width) 0 (3/8 * width)
      calcPixel p n = (toOrigin * radiusScale * rotate n) `mpMult` p
      points = zipWith calcPixel (replicate 12 twelveO'Clock) [0..11]
      pairs = [(round x, round z) | Point x _ z <- points]
   in setPixels white pairs c

runChapter4 :: IO ()
runChapter4 = writeCanvas "clock.ppm" (plotClock $ initCanvas 200 200)