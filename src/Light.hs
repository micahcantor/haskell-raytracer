module Light where

import Color (scale)
import GHC.IO.Unsafe (unsafePerformIO)
import Material (black, defaultMaterial, patternAtShape)
import System.Random (randomRIO)
import Types
  ( Color,
    Light (..),
    Material (..),
    Pattern (..),
    Point (..),
    Shape (..),
    Vec (..),
    (~=),
  )
import VecPoint (dot, normalize, pAdd, pMult, pSub, reflect, vAdd, vDiv, vMult, vNeg, vpAdd)
import Debug.Trace (traceShow)

lighting :: Material -> Shape -> Light -> Point -> Vec -> Vec -> Double -> Color
lighting material shape light point eyev normalv intensity =
  traceShow ("positions: " ++ show positions)
  traceShow ("lightvs: " ++ show lightvs)
  traceShow ("lightDotNormals: " ++ show lightDotNormals)
  ambientLight + totalDiffuse + totalSpecular
  where
    -- find color of surface if the material is patterned
    surfaceColor = case pattern material of
      Nothing -> color material
      Just p -> patternAtShape p shape point
    -- combine surface color and light's color
    effectiveColor = surfaceColor * lightColor light
    -- compute the ambient contribution
    ambientLight = ambient material `scale` effectiveColor
    samples = fromIntegral $ case light of
      PointLight {} -> 1
      AreaLight {samples} -> samples
    positions = samplePoints light
    -- find the direction to the light source
    lightvs = map (\pos -> normalize (pos `pSub` point)) positions
    -- lightDotNormal represents the cosine of the angle between the light vector
    -- and the normal vector. Negative value means light is on the other side of surface.
    lightDotNormals = map (`dot` normalv) lightvs
    calcDiffuseLighting lightDotNormal
      | lightDotNormal < 0 = black
      | otherwise =
        let scalar = diffuse material * lightDotNormal * intensity
         in scalar `scale` effectiveColor
    calcSpecularLighting lightDotNormal lightv
      | lightDotNormal < 0 = black
      | reflectDotEye < 0 = black
      | otherwise = scalar `scale` lightColor light
      where
        reflectv = reflect (vNeg lightv) normalv
        reflectDotEye = reflectv `dot` eyev
        factor = reflectDotEye ** shininess material
        scalar = factor * specular material * intensity
    diffuseLights = map calcDiffuseLighting lightDotNormals
    specularLights = zipWith calcSpecularLighting lightDotNormals lightvs
    totalDiffuse = (1 / samples) `scale` sum diffuseLights
    totalSpecular = (1 / samples) `scale` sum specularLights
      
areaLight :: Point -> (Vec, Int) -> (Vec, Int) -> Bool -> Color -> Light
areaLight corner (ufull, usteps) (vfull, vsteps) jitter color =
  let uvec = ufull `vDiv` fromIntegral usteps
      vvec = vfull `vDiv` fromIntegral vsteps
      samples = usteps * vsteps
   in AreaLight
        { corner = corner,
          uvec = uvec,
          vvec = vvec,
          usteps = usteps,
          vsteps = vsteps,
          samples = samples,
          jitter = jitter,
          lightColor = color
        }

pointOnLight :: Light -> Int -> Int -> Point
-- when the area light has random jitter turned on,
-- this is not referentially transparent.
pointOnLight AreaLight {corner, uvec, vvec, jitter} u v =
  let offset_u
        | jitter = unsafePerformIO (randomRIO (0.0, 1.0))
        | otherwise = 0.5
      offset_v
        | jitter = unsafePerformIO (randomRIO (0.0, 1.0))
        | otherwise = 0.5
      v1 = (fromIntegral u + offset_u) `vMult` uvec
      v2 = (fromIntegral v + offset_v) `vMult` vvec
   in corner `vpAdd` (v1 `vAdd` v2)

samplePoints :: Light -> [Point]
samplePoints PointLight {position} = [position]
samplePoints light@AreaLight {usteps, vsteps} =
  [ pointOnLight light u v
    | u <- [0 .. usteps - 1],
      v <- [0 .. vsteps - 1]
  ]