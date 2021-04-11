module Light where

import Color (Color (..), cAdd, cMult, hadamard)
import Material (Material (..), color, defaultMaterial)
import VecPoint (Point (..), Vec (..), dot, normalize, pSub, reflect, vNeg)

data PointLight = PointLight {position :: Point, intensity :: Color}

lighting :: Material -> PointLight -> Point -> Vec -> Vec -> Bool -> Color
lighting material light point eyev normalv inShadow =
  ambientLight `cAdd` diffuseLight `cAdd` specularLight
  where
    black = Color 0 0 0
    -- combine surface color and light's color
    effectiveColor = color material `hadamard` intensity light
    -- find the direction to the light source
    lightv = normalize (position light `pSub` point)
    -- compute the ambient contribution
    ambientLight = ambient material `cMult` effectiveColor
    -- lightDotNormal represents the cosine of the angle between the light vector
    -- and the normal vector. Negative value means light is on the other side of surface.
    lightDotNormal = lightv `dot` normalv
    diffuseLight
      | lightDotNormal < 0 = black
      | inShadow = black
      | otherwise = (diffuse material * lightDotNormal) `cMult` effectiveColor
    specularLight
      | lightDotNormal < 0 = black
      | reflectDotEye < 0 = black
      | inShadow = black
      | otherwise = (factor * specular material) `cMult` intensity light
      where
        reflectv = reflect (vNeg lightv) normalv
        -- reflectDotEye represents the cosine of the angle between the reflect vector
        -- and the eye vector. Negative value means light is on the other side of surface.
        reflectDotEye = reflectv `dot` eyev
        factor = reflectDotEye ** shininess material
