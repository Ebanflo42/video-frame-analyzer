module Main where

import Control.Parallel.Strategies

import Data.STBImage

import Persistence

import System.Process
import System.IO

main = do
  putStrLn "Input the video's file name."
  path <- getLine
  putStrLn "Input desired sample rate (FPS)."
  fps <- getLine
  readProcess "ffmpeg" ["-i", path, "-vf", "fps=" ++ fps, "test-%03d.png"] "" >>= putStrLn
  return ()