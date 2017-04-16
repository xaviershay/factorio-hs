module Main where

import System.Environment (getArgs)
import System.Exit (die)

import Lib

main :: IO ()
main = do
  args <- getArgs
  (factorioPath, inputFile) <-
    case args of
      [x, y] -> return (x, y)
      _ -> die "Usage: factorio-solve FACTORIO_PATH INPUT_DOT"

  -- Load dotfile and data
  graph <- loadDotFile inputFile
  productionData <- loadFactorioData factorioPath

  ---- Solve graph
  solved <- solveDot productionData graph

  ------ Write new dotfile to stdout
  putDotGraph solved
