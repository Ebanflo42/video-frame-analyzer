module Main where

import Data.STBImage
import Data.Vector as V
import Data.List as L

import Persistence hiding (bottleNeckDistance, bottleNeckDistances)

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


-- | The standard (Euclidean) metric between index barcodes.
indexMetric :: BarCode -> BarCode -> Extended Double
indexMetric (_, Just _) (_, Nothing)    = Infinity
indexMetric (_, Nothing) (_, Just _)    = Infinity
indexMetric (i, Nothing) (j, Nothing)   =
  Finite $ fromIntegral $ abs $ i - j
indexMetric (i, Just j) (k, Just l) =
  let x = i - k; y = j - l
  in Finite $ sqrt $ fromIntegral $ x*x + y*y

{- |
  Given a metric, return the maximum of minimum distances bewteen the bar codes.
  Returns noting if either list of barcodes is empty.
-}
bottleNeckDistance :: Ord b => (BarCode -> BarCode -> Extended b) -> [BarCode] -> [BarCode] -> Maybe (Extended b)
bottleNeckDistance metric diagram1 diagram2
  | L.null diagram1 = Nothing
  | L.null diagram2 = Nothing
  | otherwise       =
    let first  = L.maximum $ L.map (\p -> L.minimum $ L.map (metric p) diagram2) diagram1
        second = L.maximum $ L.map (\p -> L.minimum $ L.map (metric p) diagram1) diagram2
    in Just $ max first second

-- |  Get's all the bottleneck distances; a good way to determine the similarity of the topology of two filtrations.
bottleNeckDistances :: Ord b => (BarCode -> BarCode -> Extended b) -> [[BarCode]] -> [[BarCode]] -> [Maybe (Extended b)]
bottleNeckDistances metric diagrams1 diagrams2 =
  let d = (L.length diagrams1) - (L.length diagrams2)
  in
    if d >= 0 then (L.zipWith (bottleNeckDistance metric) diagrams1 diagrams2) L.++ (L.replicate d Nothing)
    else (L.zipWith (bottleNeckDistance metric) diagrams1 diagrams2) L.++ (L.replicate (-d) Nothing)

scales :: [Float]
scales =
  [ 0.039
  , 0.038
  , 0.037
  , 0.036
  , 0.035
  , 0.034
  , 0.033
  , 0.032
  , 0.031
  , 0.03
  ]

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
  let filtrations = parMapVec (makeVRFiltrationLight scales metric) pcd
  let barcodes    = parMapVec ((L.take 2) . persistentHomology) filtrations
  let distances   = parMapWithIndex (\i d -> bottleNeckDistances indexMetric d (barcodes ! (i + 1))) $ V.tail barcodes

  V.foldl1 (>>) $ V.map (putStrLn . show) distances