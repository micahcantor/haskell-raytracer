module World where

import Light ( PointLight )
import Sphere ( Sphere )

data World = World {light :: PointLight, objects :: [Sphere]}

defaultWorld :: World
defaultWorld =
  let light = PointLight (Point (-10) 10 (-10)) (Color 1 1 1)
      s1 = untSphere {}