module Canvas where

import Types ( Color(..), Canvas )
import Color ( cMult )
import qualified Color (toPPM)
import Data.Matrix (Matrix (..), matrix, setElem, toLists, getElem)
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

initCanvas :: Int -> Int -> Canvas
initCanvas width height = matrix height width (\_ -> Color 0 0 0)

setPixel :: Color -> Int -> Int -> Canvas -> Canvas
setPixel co x y = setElem co (y + 1, x + 1) -- Data.Matrix is 1-indexed

canvasHeight :: Canvas -> Int
canvasHeight = nrows

canvasWidth :: Canvas -> Int
canvasWidth = ncols

canvasDimensions :: Canvas -> (Int, Int)
canvasDimensions c = (canvasWidth c, canvasHeight c)

(!) :: Canvas -> (Int, Int) -> Color
canvas ! (x, y) = getElem (y + 1) (x + 1) canvas

toPPM :: Canvas -> String
toPPM c = header ++ body
  where
    maxColor = 255
    w = show (canvasWidth c)
    h = show (canvasHeight c)
    header = unlines ["P3", w ++ " " ++ h, show maxColor]
    rowToString = unwords . map (Color.toPPM maxColor)
    body = unlines $ map rowToString (toLists c)

writeCanvas :: FilePath -> Canvas -> IO ()
writeCanvas fp = writeFile fp . toPPM