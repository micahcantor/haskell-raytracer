module Canvas where

import Types ( Color(..), Canvas )
import Color ( cMult, toPPM )
import Data.Matrix (Matrix (..), matrix, setElem, toLists, getElem)
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

initCanvas :: Int -> Int -> Canvas
initCanvas width height = matrix height width (\_ -> Color 0 0 0)

setPixel :: Color -> Int -> Int -> Canvas -> Canvas
setPixel co x y = setElem co (y, x)

canvasHeight :: Canvas -> Int
canvasHeight = nrows

canvasWidth :: Canvas -> Int
canvasWidth = ncols

canvasDimensions :: Canvas -> (Int, Int)
canvasDimensions c = (canvasWidth c, canvasHeight c)

(!) :: Canvas -> (Int,  Int) -> Color
canvas ! (x, y) = getElem y x canvas

writeCanvas :: FilePath -> Canvas -> IO ()
writeCanvas fp c = do
  let maxColor = 255
      w = show (canvasWidth c)
      h = show (canvasHeight c)
      header = unwords ["P3", "\n", w, h, "\n", show maxColor]
      rowToString = unwords . map (toPPM maxColor . cMult (fromIntegral maxColor))
      ppmLines = map rowToString (toLists c)
  withFile
    fp
    WriteMode
    ( \handle -> do
        hPutStrLn handle header
        mapM_ (hPutStrLn handle) ppmLines
    )