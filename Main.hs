module Main where

import Control.Parallel.Strategies

import Data.STBImage
import Data.Vector as V
import Data.List as L

import Persistence

import System.Process
import System.IO

dropEnd :: Int -> [a] -> [a]
dropEnd 0 list = list
dropEnd i list = dropEnd (i - 1) $ L.init list

-- | Generate a range of integers in vector form.
range :: Int -> Int -> Vector Int
range x y
  | x == y = x `cons` empty
  | x < y  = x `cons` (range (x + 1) y)
  | x > y  = (range x (y + 1)) `snoc` y

readImages :: String -> Int -> IO (Vector (Image RGBColor))
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