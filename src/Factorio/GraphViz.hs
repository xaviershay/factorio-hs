{-# LANGUAGE OverloadedStrings #-}

module Factorio.GraphViz
  ( loadDotFile
  , putDotGraph
  , solveDot
  ) where

import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.IO    as TL

import Data.Maybe  (fromJust, fromMaybe)
import Data.Monoid ((<>))

import Data.GraphViz
    ( DotEdge (..)
    , DotGraph (..)
    , DotNode (..)
    , PrintDot (..)
    , PrintDotRepr
    , graphEdges
    , graphNodes
    , parseDotGraph
    , printDotGraph
    )
import Data.GraphViz.Attributes.Complete
    (Attribute (UnknownAttribute), customAttribute)
import Data.GraphViz.Types.Canonical     (DotStatements (DotStmts), nodeStmts)

import Factorio.Solver (solve)
import Factorio.Types

loadDotFile :: FilePath -> IO (DotGraph String)
loadDotFile path = parseDotGraph <$> TL.readFile path

putDotGraph :: (Ord a, PrintDot a) => DotGraph a -> IO ()
putDotGraph = TL.putStrLn . printDotGraph

dotToProductionGraph
  :: (Ord a)
  => ProductionData -> DotGraph a -> G.Gr (DotNode a) (DotEdge a)
dotToProductionGraph recipes dg = G.mkGraph ns es
  where
    nodeName (DotNode n _) = n
    ns = zip [0 ..] (graphNodes dg)
    nodeTable = M.fromList . map (\(x, y) -> (nodeName y, x)) $ ns
    es = map (toE nodeTable) $ graphEdges dg
    -- TODO: Remove use of fromJust
    toE table e@(DotEdge from to as) =
      (fromJust $ M.lookup from table, fromJust $ M.lookup to table, e)

mapNodes :: (DotNode a -> DotNode a) -> DotGraph a -> DotGraph a
mapNodes f x@DotGraph {graphStatements = y@DotStmts {nodeStmts = ns}} =
  x {graphStatements = y {nodeStmts = map f ns}}

solveDot :: ProductionData -> DotGraph String -> IO (DotGraph String)
solveDot recipes dot = do
  x <- s
  return (dotAugment dot x)
  where
    inputGraph = dotToProductionGraph recipes dot
    s = solve recipes toPN toPE inputGraph
    toPN :: DotNode String -> ProductionNode
    toPN (DotNode recipeName as) =
      case M.lookup (T.pack recipeName) recipes of
        Just r -> ProductionNode r (targetFrom as) ()
        Nothing -> error $ "recipe not found: " <> recipeName
    toPE :: DotEdge String -> Item
    toPE (DotEdge from to as) = T.pack from
    targetFrom as =
      fromMaybe Auto (firstJust $ map extractTarget as)
    extractTarget (UnknownAttribute "factorioTarget" t) =
      Just $ Target (read . TL.unpack $ t)
    extractTarget _ = Nothing
    firstJust (Just x:xs) = Just x
    firstJust (_:xs) = firstJust xs
    firstJust [] = Nothing

dotAugment :: DotGraph String
           -> M.Map (DotNode String) Double
           -> DotGraph String
dotAugment existing rates = mapNodes addSolution existing
  where
    addSolution n@(DotNode name as) =
      case M.lookup n rates of
        Just rate ->
          DotNode
            name
            (customAttribute "factorioRate" (TL.pack . show $ rate) : as)
        Nothing -> n
