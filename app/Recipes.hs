module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit        (die)

import Factorio

main :: IO ()
main = do
  args <- getArgs
  factorioPath <-
    case args of
      [x] -> return x
      _   -> die "Usage: factorio-recipes FACTORIO_PATH"

  productionData <- loadFactorioData factorioPath

  forM_ productionData $ putStrLn . show
