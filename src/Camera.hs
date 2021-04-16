module Camera where

import Types
    ( Point(Point), World, Ray(..), Canvas, Camera(Camera) )
import Canvas (initCanvas)
import Transformation (identity, inverse, mpMult)
import VecPoint (normalize, pSub)
import World (colorAt)
import Data.Matrix (mapPos)

defaultCamera :: Camera
defaultCamera = Camera 160 120 (pi / 2) identity

viewHalfDimensions :: Camera -> (Float, Float)
viewHalfDimensions (Camera hSize vSize fov _) =
  let halfView = tan (fov / 2)
      aspect = fromIntegral hSize / fromIntegral vSize
      halfWidth
        | aspect >= 1 = halfView
        | otherwise = halfView * aspect
      halfHeight
        | aspect >= 1 = halfView / aspect
        | otherwise = halfView
   in (halfWidth, halfHeight)

pixelSize :: Int -> Float -> Float
pixelSize hSize halfWidth = (halfWidth * 2) / fromIntegral hSize

rayForPixel :: Camera -> Int -> Int -> Ray
rayForPixel c@(Camera hSize vSize fov transform) px py =
  let (halfWidth, halfHeight) = viewHalfDimensions c
      pixelSize = Camera.pixelSize hSize halfWidth
      xOffset = (fromIntegral px + 0.5) * pixelSize
      yOffset = (fromIntegral py + 0.5) * pixelSize
      worldX = halfWidth - xOffset
      worldY = halfHeight - yOffset
      transformInverse = inverse transform
      pixel = transformInverse `mpMult` Point worldX worldY (-1)
      origin = transformInverse `mpMult` Point 0 0 0
      direction = normalize (pixel `pSub` origin)
   in Ray origin direction

render :: Camera -> World -> Canvas
render camera@(Camera hSize vSize _ _) world =
  let canvas = initCanvas hSize vSize
   in mapPos
        (\(y, x) _ -> colorAt world (rayForPixel camera x y))
        canvas