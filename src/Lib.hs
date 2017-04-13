{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc
  ) where

import Data.Text

import Control.Monad.LPMonad
import Data.LinearProgram hiding ((-), (/))
import Data.Monoid -- (<>)

import qualified Data.Graph.Inductive as G

type Item = Text
type AssemblerSpec = () -- TODO

data RateSpec = Auto | Target Double
data ProductionNode = ProductionNode Recipe RateSpec AssemblerSpec

type ProductionGraph = G.Gr ProductionNode Item

data Recipe = Recipe
  { name :: Text
  , duration :: Double
  , items :: [(Item, Double)]
  } deriving (Show)

mkRecipe n d is = Recipe {name = n, duration = d, items = is}

n *& v = linCombination [(n, v)]

lp :: ProductionGraph -> LP Text Double
lp graph =
  execLPM $ do
    setDirection Min
    mapM_ addConstraintsForNode blah
    setObjective $ linCombination [(1, "0"), (1, "1")]
    --equal' "node-ratio" (2 *& "node") (1 *& "iron-plate")
    --equal' "node-ratio" ((-1) *& "node") (1 *& "iron-ore")
    mapM (equalTo (1 *& "1-iron-plate")) [(100)]
  where
    blah = G.ufold f [] graph
    f (_, n, node, out) xs = (n, node, out):xs
    addConstraintsForNode (n,
      (ProductionNode (Recipe { duration = d, items = is }) rateSpec _),
      links) = do
                mapM_ (\(i, r) -> equal' ("node-" <> show n <> "-ratio") (r *& (pack . show) n) (1 *& ((pack . show) n <> "-" <> i))) is
                mapM_ (\(item, targetN) -> equal' "link" (1 *& ((pack . show) n <> "-" <> item)) ((-1) *& ((pack . show) targetN <> "-" <> item))) links

testingGraph :: ProductionGraph
testingGraph = G.mkGraph nodes edges
  where
    nodes = [ (0, ProductionNode miner Auto ())
            , (1, ProductionNode furnace (Target 10) ())
            ]
    edges = [(0, 1, "iron-ore")]

miner = mkRecipe "miner" 1 [("iron-ore", 1)]
furnace = mkRecipe "furnace" 1 [("iron-ore", (-1)), ("iron-plate", 2)]

pretty LP { constraints = cs } = intercalate "\n" $ Prelude.map (pack . show) cs

someFunc :: IO ()
someFunc =
  let r = furnace
  in do putStrLn . unpack $ pretty (lp testingGraph)
        print =<< glpSolveVars mipDefaults (lp testingGraph)
