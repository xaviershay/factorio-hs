{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Data.Text

import Control.Monad.LPMonad
import Data.LinearProgram hiding ((-), (/))

type Item = Text

data Recipe = Recipe
  { name :: Text
  , duration :: Double
  , items :: [(Item, Integer)]
  } deriving (Show)

mkRecipe n d is = Recipe {name = n, duration = d, items = is}

n *& v = linCombination [(n, v)]

lp :: LP String Double
lp =
  execLPM $ do
    setDirection Min
    setObjective $ linCombination [(1, "iron-plate")]
    equal' "node-ratio" (1 *& "node") (0.5 *& "iron-plate")
    equal' "node-ratio" (1 *& "node") ((1 / (-1)) *& "iron-ore")
    equalTo (1 *& "iron-ore") (-100)

someFunc :: IO ()
someFunc =
  let r = mkRecipe "furnace" 1 [("iron-ore", (-1)), ("iron-plate", 2)]
  in do putStrLn $ show lp
        print =<< glpSolveVars mipDefaults lp
