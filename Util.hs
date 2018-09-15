module Util where

import Control.Parallel.Strategies

import Data.Bits
import Data.Word
import Data.STBImage
import Data.Vector.Storable as SV
import Data.Vector as V
import Data.List as L

type FloatingImage = (V.Vector (Float, Float, Float), Int, Int)

metric :: Floating a => (a, a, a, a, a) -> (a, a, a, a, a) -> a
metric (x1, x2, x3, x4, x5) (y1, y2, y3, y4, y5) =
  let d1 = y1 - x1; d2 = y2 - x2; d3 = y3 - x3; d4 = y4 - x4; d5 = y5 - x5
  in sqrt $ d1*d1 + d2*d2 + d3*d3 + d4*d4 + d5*d5

dropEnd :: Int -> [a] -> [a]
dropEnd 0 list = list
dropEnd i list = dropEnd (i - 1) $ L.init list

-- | Generate a range of integers in vector form.
range :: Int -> Int -> V.Vector Int
range x y
  | x == y = x `V.cons` V.empty
  | x < y  = x `V.cons` (range (x + 1) y)
  | x > y  = (range x (y + 1)) `V.snoc` y

-- | Parallel map a function over a vector.
parMapVec :: (a -> b) -> V.Vector a -> V.Vector b
parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

-- | Map a function that takes into account the index of each element in parallel.
parMapWithIndex :: (Int -> a -> b) -> V.Vector a -> V.Vector b
parMapWithIndex f vector =
  let helper i vec = runEval $
        if V.null vec then return V.empty
        else
          let current = f i $ V.head vec; rest = helper (i + 1) $ V.tail vec
          in rpar current >> rseq rest >> (return $ current `V.cons` rest)
  in helper 0 vector

convert2Vector :: Storable a => SV.Vector a -> V.Vector a
convert2Vector vector =
  if SV.null vector then V.empty
  else (SV.head vector) `V.cons` (convert2Vector $ SV.tail vector)

convert2Storable :: Storable a => V.Vector a -> SV.Vector a
convert2Storable vector =
  if V.null vector then SV.empty
  else (V.head vector) `SV.cons` (convert2Storable $ V.tail vector)

img2Floating :: Image RGBColor -> FloatingImage
img2Floating (Image pixels width height) =
  let convertColor (RGBColor r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)
  in (V.map convertColor $ convert2Vector pixels, width, height)

-- | Average groups of 2x2 pixels. 
downSample :: Image RGBColor -> Image RGBColor
downSample (Image pixels w h) =
  let w' = w `shiftR` 1; h' = h `shiftR` 1

      avg :: RGBColor -> RGBColor -> RGBColor -> RGBColor -> RGBColor
      avg (RGBColor r1 g1 b1) (RGBColor r2 g2 b2) (RGBColor r3 g3 b3) (RGBColor r4 g4 b4) =
        RGBColor ((r1 + r2 + r3 + r4) `shiftR` 2) ((g1 + g2 + g3 + g4) `shiftR` 2) ((b1 + b2 + b3 + b4) `shiftR` 2)

      makeNewPixels r x y =
        let yw     = y*w
            x1     = x + 1
            pixel1 = pixels SV.! (x + yw)
            pixel2 = pixels SV.! (x1 + yw)
            yw2    = yw + w
            pixel3 = pixels SV.! (x + yw2)
            pixel4 = pixels SV.! (x1 + yw2)
        in
          if x >= w then
            if y >= h then SV.empty
            else makeNewPixels r 0 (y + 2)
          else makeNewPixels (r `SV.snoc` (avg pixel1 pixel2 pixel3 pixel4)) (x + 2) y

  in Image (makeNewPixels SV.empty 0 0) w' h'