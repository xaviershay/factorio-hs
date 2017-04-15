{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc
  ) where

import Debug.Trace
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe
import Control.Monad.LPMonad
import Data.LinearProgram hiding ((-), (/))
import Data.Monoid -- (<>)

import qualified Data.Graph.Inductive as G

import           System.Directory
import System.FilePath

import qualified Scripting.Lua as Lua
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Aeson
import Data.Aeson.Types
import Scripting.Lua.Aeson
import Control.Applicative

import Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GVAC

type Item = T.Text
type AssemblerSpec = () -- TODO

data RateSpec = Auto | Target Double deriving (Show)
data ProductionNode = ProductionNode Recipe RateSpec AssemblerSpec deriving (Show)

type ProductionGraph = G.Gr ProductionNode Item

data Recipe = Recipe
  { name :: T.Text
  , duration :: Double
  , items :: [Ingredient]
  , category :: T.Text
  } deriving (Show)

data Ingredient = Ingredient Item Double deriving (Show)

instance FromJSON Ingredient where
  parseJSON x = case x of
    Object o -> Ingredient
                  <$> o .: "name"
                  <*> o .: "amount"

    Array _ -> do
      (n, a) <- parseJSON x

      return $ Ingredient n a

    invalid -> typeMismatch "Ingredient" invalid

parseResult v = do
  item  <- v .: "result"
  count <- v .:? "result_count" .!= 1.0

  return [Ingredient item count]

instance FromJSON Recipe where
  parseJSON (Object v) = do
    n        <- v .: "name"
    inputs   <- v .: "ingredients"
    outputs  <- (v .: "results") <|>
                parseResult v <|>
                fail "Recipe must have one of: results, result"
    category <- v .:? "category" .!= "crafting"

    return $ Recipe {
                 name = n
               , duration = 1
               , items = map (\(Ingredient n a) -> Ingredient n (-a)) inputs <> outputs
               , category = category
               }

-- ("diesel-locomotive",Object (fromList [("enabled",Bool False),("in gredients",Array [Array [String "engine-unit",Number 20.0],Array [String "electronic-circu it",Number 10.0],Array [String "steel-plate",Number 30.0]]),("result",String "diesel-locom otive"),("name",String "diesel-locomotive"),("type",String "recipe")]))

mkRecipe n d is = Recipe {
  name = n
  , duration = d
  , items = is
  , category = "crafting"
  }

n *& v = linCombination [(n, v)]

lp :: ProductionGraph -> LP T.Text Double
lp graph =
  execLPM $ do
    setDirection Min
    mapM_ addConstraintsForNode blah
  where
    blah = G.ufold f [] graph
    f (inlinks, n, node, out) xs = (n, node, inlinks, out):xs
    addConstraintsForNode (n,
      (ProductionNode (Recipe { duration = d, items = is }) rateSpec _),
      incomingLinks,
      outgoingLinks) = do
                mapM_ (\(Ingredient i r) -> equal' ("node-" <> show n <> "-ratio") (r *& (T.pack . show) n) (1 *& ((T.pack . show) n <> "-" <> i))) is
                mapM_ (\(item, targetN) -> addLink n targetN item) outgoingLinks
                mapM_ (\(item, sourceN) -> addLink sourceN n item) incomingLinks

                case rateSpec of
                  Target t ->
                    equalTo' ("node-" <> show n <> "-target") (1 *& (T.pack . show) n) t
                  Auto     -> setObjective (1 *& ((T.pack . show) n))

-- Supply must be less than consumption
addLink from to item = do
  geq' "link"    (1 *& ((T.pack . show) from <> "-" <> item))
                ((-1) *& ((T.pack . show) to <> "-" <> item))

miner = mkRecipe "miner" 1 [Ingredient "iron-ore" 1]
furnace = mkRecipe "furnace" 1 [Ingredient "iron-ore" (-1), Ingredient "iron-plate" 2]

pretty LP { constraints = cs } = T.intercalate "\n" $ map (T.pack . show) cs

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = do
  e <- doesDirectoryExist path
  if e
    then do
      files <- listDirectory path
      let fullDirs = map (\x -> joinPath [path, x]) files
      dirs <- filterM doesDirectoryExist fullDirs

      return dirs
    else return []

dataPath :: String
dataPath = "/home/xavier/data/factorio/data"
packagePath = dataPath <> "/core/lualib"

addPackagePath l p = do
  Lua.loadstring l ("package.path = package.path .. ';" <> p <> "/?.lua'")
  Lua.call l 0 0


loadLua = do
  l <- Lua.newstate
  e <- Lua.openlibs l
  putStrLn $ show e
  e <- addPackagePath l packagePath
  putStrLn $ show e
  --Lua.loadstring l "def = {}; def.abc = \"hello\";"
  --Lua.call l 0 0
  --putStrLn "------------"
  Lua.getglobal l "package"
  Lua.getfield l (-1) "path"

  x <- Lua.peek l (-1)
  putStrLn $ show (x :: Maybe BS.ByteString)

  e <- Lua.loadfile l $ dataPath <> "/core/lualib/dataloader.lua"
  putStrLn $ show e
  e <- Lua.call l 0 0
  putStrLn $ show e
  e <- Lua.loadfile l "patch.lua"
  putStrLn $ show e
  e <- Lua.call l 0 0
  putStrLn $ show e
  dirs <- listDirectories dataPath
  forM_ dirs $ \modDir -> do
    putStrLn modDir
    -- TODO Reset package path
    -- TODO Clear loaded table
    addPackagePath l modDir

    e <- Lua.loadfile l (joinPath [modDir, "data.lua"])
    putStrLn $ show e
    e <- Lua.call l 0 0
    putStrLn $ show e

  size <- Lua.gettop l
  putStrLn $ show size
  Lua.getglobal l "data"
  Lua.getfield l (-1) "raw"
  Lua.getfield l (-1) "recipe"
  --Lua.getfield l (-1) "diesel-locomotive"
  --Lua.getfield l (-1) "small-electric-pole"

  x <- Lua.istable l (-1)
  putStrLn $ show x

  rs <- Lua.peek l (-1)
  --putStrLn . show $ rs

  case parseEither parseJSON (fromJust rs) :: Either String (M.Map String Recipe) of
    Left e -> putStrLn e
    Right recipes -> do
      let gvsrc = "digraph {\"copper-cable\" [factorioTarget = 5]; \"copper-plate\" -> \"copper-cable\"}"
      let g = (GV.parseDotGraph gvsrc :: DotGraph String)

      putStrLn "---"
      G.prettyPrint $ dotToProductionGraph recipes g
      putStrLn "---"
      putStrLn . T.unpack . pretty . lp $ dotToProductionGraph recipes g
      print =<< glpSolveVars mipDefaults (lp $ dotToProductionGraph recipes g)
      --forM_ (M.elems recipes) putRecipeLn


putRecipeLn :: Recipe -> IO ()
putRecipeLn (Recipe { name = name }) = do
  putStrLn . T.unpack $ name

  --putStrLn . show $ ((parseEither parseJSON (fromJust rs)) :: Either String (M.Map String Recipe))
  --putStrLn . show $ ((parseEither parseJSON (fromJust rs)) :: Either String (Recipe))


dotToProductionGraph :: M.Map String Recipe -> DotGraph String -> ProductionGraph
dotToProductionGraph recipes dg = G.mkGraph ns es
  where
    ns = zip [0..] (map toPN $ GV.graphNodes dg)
    es = map (toPNE nodeTable) $ GV.graphEdges dg

    nodeTable :: M.Map String G.Node
    nodeTable = M.fromList (zip (map nodeName . GV.graphNodes $ dg) [0..])
    nodeName (DotNode n _) = n
    toPN (DotNode recipeName as) =
      case M.lookup recipeName recipes of
        Just r -> ProductionNode r (targetFrom as) ()
        Nothing -> error $ "recipe not found: " <> recipeName

    toPNE :: (M.Map String G.Node) -> DotEdge String -> (G.Node, G.Node, T.Text)
    toPNE table (DotEdge from to as) =
      (fromJust $ M.lookup from table,
       fromJust $ M.lookup to table, T.pack from)

    targetFrom as = case firstJust $ map extractTarget as of
                      Just t  -> t
                      Nothing -> Auto
    extractTarget (GVAC.UnknownAttribute "factorioTarget" t) = Just $ Target (read . TL.unpack $ t)
    extractTarget _ = Nothing

    firstJust (Just x:xs) = Just x
    firstJust (_:xs) = firstJust xs
    firstJust []     = Nothing


testingGraph :: ProductionGraph
testingGraph = G.mkGraph nodes edges
  where
    nodes = [ (0, ProductionNode miner Auto ())
            , (1, ProductionNode furnace (Target 10) ())
            ]
    edges = [(0, 1, "iron-ore")]

someFunc :: IO ()
someFunc = loadLua
--someFunc = testDot

someFunc2 :: IO ()
someFunc2 =
  let r = furnace
  in do putStrLn . T.unpack $ pretty (lp testingGraph)
        print =<< glpSolveVars mipDefaults (lp testingGraph)
