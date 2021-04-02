module Color where

data Color = Color Float Float Float

instance Show Color where
    show (Color r g b) = show r ++ " " ++ show g ++ " " ++ show b

cAdd :: Color -> Color-> Color
cAdd (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (b1 + b2) (g1 + g2)

cSub :: Color -> Color -> Color
cSub (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 - r2) (b1 - b2) (g1 - g2)

cMult :: Float -> Color -> Color
cMult c (Color r g b) = Color (c * r) (c * g) (c * b)

hadamard :: Color -> Color -> Color
hadamard (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)