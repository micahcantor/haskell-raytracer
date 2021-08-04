module Light where

import Material (black, defaultMaterial, patternAtShape)
import Types
  ( Color,
    Material (ambient, color, diffuse, pattern, shininess, specular),
    Pattern (..),
    Point,
    PointLight (intensity, position),
    Vec,
    Shape(..)
  )
import VecPoint (dot, normalize, pSub, reflect, vNeg)
import Color (scale)

lighting :: Material -> Shape -> PointLight -> Point -> Vec -> Vec -> Bool -> Color
lighting material shape light point eyev normalv inShadow =
  ambientLight + diffuseLight + specularLight
  where
    -- find color of surface if the material is patterned
    surfaceColor = case pattern material of
      Nothing -> color material
      Just p -> patternAtShape p shape point
    -- combine surface color and light's color
    effectiveColor = surfaceColor * intensity light
    -- find the direction to the light source
    lightv = normalize (position light `pSub` point)
    -- compute the ambient contribution
    ambientLight = ambient material `scale` effectiveColor
    -- lightDotNormal represents the cosine of the angle between the light vector
    -- and the normal vector. Negative value means light is on the other side of surface.
    lightDotNormal = lightv `dot` normalv
    diffuseLight
      | lightDotNormal < 0 = black
      | inShadow = black
      | otherwise = (diffuse material * lightDotNormal) `scale` effectiveColor
    specularLight
      | lightDotNormal < 0 = black
      | reflectDotEye < 0 = black
      | inShadow = black
      | otherwise = (factor * specular material) `scale` intensity light
      where
        reflectv = reflect (vNeg lightv) normalv
        -- reflectDotEye represents the cosine of the angle between the reflect vector
        -- and the eye vector. Negative value means light is on the other side of surface.
        reflectDotEye = reflectv `dot` eyev
        factor = reflectDotEye ** shininess material
