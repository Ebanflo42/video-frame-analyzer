module Main where

import Data.STBImage
import Data.Vector as V
import Data.List as L

import Persistence

import System.Process

import Util

instance Show a => Show (Extended a) where
  show (Finite a) = "Finite " L.++ (show a)
  show Infinity   = "Infinity"
  
readImages :: Vector String -> IO (Vector (Image RGBColor))
readImages names =
  let unsafeLoad s = loadImage RGB s >>=
        \lod -> case lod of
          Right img -> return img
          Left err  -> error err
  in V.mapM unsafeLoad names

toPCD :: FloatingImage -> [(Float, Float, Float, Float, Float)]
toPCD (pixels, width, height) =
  let wf   = fromIntegral width
      hf   = fromIntegral height
      maxI = width*height

      helper i =
        if i == maxI then []
        else
          let x = i `mod` width
              y = i `div` width
          in ((\(a,b,c) (d,e) -> (a,b,c,d,e)) (pixels ! i) ((fromIntegral x)/wf, (fromIntegral y)/hf)):(helper (i + 1))

  in helper 0

main = do

  putStrLn "Input the video's file name (must end with .webm)."
  path <- getLine
  let name = dropEnd 5 path
  putStrLn "Input desired sample rate (FPS)."
  fps <- getLine

  framesStatus <- readProcess "ffmpeg" ["-i", path, "-vf", "fps=" L.++ fps, name L.++ "-%03d.png"] ""
  putStrLn framesStatus

  numPngs <- readProcess "find" ["-type", "f", "-name", "*.png", "-printf", "x"] ""

  let translate x
        | x < 10    = "00" L.++ (show x)
        | x < 100   = '0':(show x)
        | otherwise = show x

  let num    = L.length numPngs
  let names  = V.map (\s -> name L.++ ('-':s) L.++ ".png") $ V.map translate $ 1 `range` num

  V.foldl1 (>>) $ V.map (\name -> readProcess "convert" [name, "-resize", "48x32", "new" L.++ name] "") names

  imgs <- readImages (V.map ((L.++) "new") names)

  let pcd         = V.map (toPCD . img2Floating) imgs
  let filtrations = parMapVec (makeVRFiltrationLight [0.3, 0.27, 0.24, 0.21, 0.18, 0.15, 0.12, 0.09, 0.06, 0.03] metric) pcd
  let barcodes    = parMapVec ((L.take 2) . persistentHomology) filtrations
  let distances   = parMapWithIndex (\i d -> bottleNeckDistances d (barcodes ! (i + 1))) $ V.tail barcodes

  V.foldl1 (>>) $ V.map (putStrLn . show) distances