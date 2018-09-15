module Main where

import Control.Parallel.Strategies

import Data.Word
import Data.STBImage
import Data.Vector as V
import Data.List as L

import Persistence

import System.Process
import System.IO

import Util

readImages :: Vector String -> IO (Vector (Image RGBColor))
readImages names =
  let unsafeLoad s = loadImage RGB s >>=
        \lod -> case lod of
          Right img -> return img
          Left err  -> error err
  in V.mapM unsafeLoad names

toPCD :: FloatingImage -> Vector (Double, Double, Double, Double, Double)
toPCD (pixels, width, height) =
  let wf   = fromIntegral width
      hf   = fromIntegral height
      maxI = width*height

      helper i =
        if i == maxI then V.empty
        else
          let x = i `mod` width
              y = i `div` width
          in ((\(a,b,c) (d,e) -> (a,b,c,d,e)) (pixels ! i) ((fromIntegral x)/wf, (fromIntegral y)/hf)) `cons` helper (i + 1)

  in helper 0

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

  let translate x
        | x < 10    = "00" L.++ (show x)
        | x < 100   = '0':(show x)
        | otherwise = show x

  readFile "output.txt"
    >>= \s -> do
      let num    = read s :: Int
      let names  = V.map (\s -> "name" L.++ ('-':s) L.++ ".png") $ V.map translate $ 1 `range` num

      V.foldl1 (>>) $ V.map (\name -> readProcess "convert" [name, "-resize", "64x48", name] "") names

      readImages names >> return ()