module Util where

import Data.STBImage
import Data.Vector.Storable as SV
import Data.Vector as V
import Data.List as L

convertVector :: Storable a => SV.Vector a -> V.Vector a
convertVector vector =
  if SV.null vector then V.empty
  else (SV.head vector) `V.cons` (convertVector $ SV.tail vector)

convertImage :: Image RGBColor -> (V.Vector (Double, Double, Double), Int, Int)
convertImage (Image pixels width height) =
  let convertColor (RGBColor r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)
  in (V.map convertColor $ convertVector pixels, width, height)
