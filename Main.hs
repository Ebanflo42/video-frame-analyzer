module Main where

import Control.Parallel.Strategies

import Data.STBImage
import Data.Vector as V
import Data.List as L

import Persistence

import System.Process
import System.IO

import Util

type FloatingImage = (Vector (Double, Double, Double), Int, Int)

dropEnd :: Int -> [a] -> [a]
dropEnd 0 list = list
dropEnd i list = dropEnd (i - 1) $ L.init list

-- | Generate a range of integers in vector form.
range :: Int -> Int -> V.Vector Int
range x y
  | x == y = x `V.cons` empty
  | x < y  = x `V.cons` (range (x + 1) y)
  | x > y  = (range x (y + 1)) `V.snoc` y

average :: Floating a => V.Vector (a, a, a) -> (a, a, a)
average vector =
  let helper (x,y,z) v =
       if V.null v then (x,y,z)
       else
         let (x',y',z') = V.head v
         in helper (x + x', y + y', z + z') $ V.tail v

      n = fromIntegral $ V.length vector

      (x,y,z) = helper (0.0, 0.0, 0.0) vector

  in (x/n, y/n, z/n)

readImages :: String -> Int -> IO (V.Vector (Image RGBColor))
readImages name n =
  let translate x
        | x < 10    = "00" L.++ (show x)
        | x < 100   = '0':(show x)
        | otherwise = show x

      names = V.map (\s -> "name" L.++ ('-':s) L.++ ".png") $ V.map translate $ 1 `range` n

      unsafeLoad s = loadImage RGB s >>=
        \lod -> case lod of
          Right img -> return img
          Left err  -> error err

  in V.mapM unsafeLoad names

-- | Average groups of NxM pixels. 
downSample :: Int -> Int -> FloatingImage -> FloatingImage
downSample n m (pixels, w, h) =
  if w `mod` n /= 0 || h `mod` m /= 0 then error "Kernel does not fit nicely into image!"
  else
    let w' = w `div` n; h' = h `div` m
        wrng = 0 `range` n; hrng = 0 `range` m

        makeNewPixels result x y =
          let x1 = x*n; x2 = x1 + n
              y1 = y*m; y2 = y1 + m

              vector =
                V.foldl1 (V.++) $ V.map (\i -> V.map (\j -> pixels ! (i + j*n)) hrng) wrng
          in
            if x == w' then
              if y == h' then result
              else makeNewPixels result 0 (y + 1)
            else makeNewPixels (result `snoc` (average vector)) (x + 1) y
            
  in (makeNewPixels V.empty 0 0, w', h')

main = do
  putStrLn "Input the video's file name (must end with .webm)."
  path <- getLine
  let name = dropEnd 5 path
  putStrLn "Input desired sample rate (FPS)."
  fps <- getLine

  readProcess "ffmpeg" ["-i", path, "-vf", "fps=" L.++ fps, name L.++ "-%03d.png"] ""
    >>= putStrLn

  readProcess "find" ["-type", "f", "-name", "*.png", "-printf", "x"] ""
    >>= writeFile "output.txt"

  readFile "output.txt"
    >>= \s -> do
      let num    = read s :: Int
      let images = readImages name num
      return ()

  return ()