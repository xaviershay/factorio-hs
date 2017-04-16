{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Factorio.Solver
  ( solve
  ) where

import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import           Data.Monoid          ((<>))
import qualified Data.Text            as T

import Data.Maybe (fromJust)

import Control.Monad.LPMonad
    (equal', equalTo', execLPM, geq', setDirection, setObjective)
import Data.LinearProgram
    (Direction (..), LP, glpSolveVars, linCombination, mipDefaults)

import Factorio.Types

n *& v = linCombination [(n, v)]

solve
  :: (Ord a)
  => ProductionData
  -> (a -> ProductionNode)
  -> (b -> Item)
  -> G.Gr a b
  -> IO (M.Map a Double)
solve recipes fn fe input = do
  let problem = constructLP (G.nemap fn fe input)
  (_, result) <- glpSolveVars mipDefaults problem
  case result of
    Just (_, varMap) ->
      return . M.fromList $
      map
        (\(n, s) -> (s, fromJust $ M.lookup (T.pack . show $ n) varMap))
        (G.labNodes input)
    Nothing -> return M.empty

constructLP :: ProductionGraph -> LP T.Text Double
constructLP graph =
  execLPM $ do
    setDirection Min
    mapM_ addConstraintsForNode blah
  where
    blah = G.ufold f [] graph
    f (inlinks, n, node, out) xs = (n, node, inlinks, out) : xs
    addConstraintsForNode (n, ProductionNode Recipe {items = is} rateSpec _, incomingLinks, outgoingLinks) = do
      mapM_
        (\(Ingredient i r) ->
           equal'
             ("node-" <> show n <> "-ratio")
             (r *& (T.pack . show) n)
             (1 *& ((T.pack . show) n <> "-" <> i)))
        is
      mapM_ (\(item, targetN) -> addLink n targetN item) outgoingLinks
      mapM_ (\(item, sourceN) -> addLink sourceN n item) incomingLinks
      case rateSpec of
        Target t ->
          equalTo' ("node-" <> show n <> "-target") (1 *& (T.pack . show) n) t
        Auto -> setObjective (1 *& (T.pack . show) n)

-- Supply must be less than consumption
addLink from to item =
  geq'
    "link"
    (1 *& ((T.pack . show) from <> "-" <> item))
    ((-1) *& ((T.pack . show) to <> "-" <> item))
