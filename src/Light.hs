module Light where

import Color (scale)
import Debug.Trace (traceShow)
import Material (black, defaultMaterial, patternAtShape)
import Types
  ( Color,
    Light (..),
    Material (..),
    Pattern (..),
    Point(..),
    Shape (..),
    Vec(..),
    (~=),
  )
import VecPoint (dot, normalize, pSub, reflect, vNeg, vDiv, pAdd, pMult, vMult, vpAdd, vAdd)

lighting :: Material -> Shape -> Light -> Point -> Vec -> Vec -> Double -> Color
lighting material shape light point eyev normalv intensity =
  ambientLight + diffuseLight + specularLight
  where
    -- find color of surface if the material is patterned
    surfaceColor = case pattern material of
      Nothing -> color material
      Just p -> patternAtShape p shape point
    -- combine surface color and light's color
    effectiveColor = surfaceColor * lightColor light
    -- find the direction to the light source
    lightv = normalize (position light `pSub` point)
    -- compute the ambient contribution
    ambientLight = ambient material `scale` effectiveColor
    -- lightDotNormal represents the cosine of the angle between the light vector
    -- and the normal vector. Negative value means light is on the other side of surface.
    lightDotNormal = lightv `dot` normalv
    diffuseLight
      | lightDotNormal < 0 = black
      | otherwise =
        let scalar = diffuse material * lightDotNormal * intensity
         in scalar `scale` effectiveColor
    specularLight
      | lightDotNormal < 0 = black
      | reflectDotEye < 0 = black
      | otherwise =
        let scalar = factor * specular material * intensity
         in scalar `scale` lightColor light
      where
        reflectv = reflect (vNeg lightv) normalv
        -- reflectDotEye represents the cosine of the angle between the reflect vector
        -- and the eye vector. Negative value means light is on the other side of surface.
        reflectDotEye = reflectv `dot` eyev
        factor = reflectDotEye ** shininess material

areaLight :: Point -> (Vec, Int) -> (Vec, Int) -> Color -> Light
areaLight corner (ufull, usteps) (vfull, vsteps) color =
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
          lightColor = color,
          position = Point 1 0 0.5
        }

pointOnLight :: Light -> Double -> Double -> Point
-- return the middle cell on an area light
pointOnLight AreaLight {corner, uvec, vvec} u v =
  let v1 = (u + 0.5) `vMult` uvec
      v2 = (v + 0.5) `vMult` vvec
   in corner `vpAdd` (v1 `vAdd` v2)
  