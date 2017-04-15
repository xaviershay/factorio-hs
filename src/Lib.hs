{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( someFunc
  ) where

import qualified Data.Text as T

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
import qualified Data.Aeson as Aeson
import Data.Aeson.Types
import Scripting.Lua.Aeson

type Item = T.Text
type AssemblerSpec = () -- TODO

data RateSpec = Auto | Target Double
data ProductionNode = ProductionNode Recipe RateSpec AssemblerSpec

type ProductionGraph = G.Gr ProductionNode Item

data Recipe = Recipe
  { name :: T.Text
  , duration :: Double
  , items :: [(Item, Double)]
  } deriving (Show)

toTuple :: Aeson.Value -> Parser (Item, Double)
toTuple (Aeson.Array x) = do
  item <- parseJSON (x V.! 0)
  amount <- parseJSON (x V.! 1)

  return (item, amount)
toTuple _ = mzero



instance Aeson.FromJSON Recipe where
  parseJSON (Aeson.Object v) = do
    n       <- v Aeson..: "name"
    inputs  <- v Aeson..: "ingredients"
    inputs' <- mapM toTuple inputs

    return $ mkRecipe n 1 inputs'

-- ("diesel-locomotive",Object (fromList [("enabled",Bool False),("in gredients",Array [Array [String "engine-unit",Number 20.0],Array [String "electronic-circu it",Number 10.0],Array [String "steel-plate",Number 30.0]]),("result",String "diesel-locom otive"),("name",String "diesel-locomotive"),("type",String "recipe")]))

mkRecipe n d is = Recipe {name = n, duration = d, items = is}

n *& v = linCombination [(n, v)]

lp :: ProductionGraph -> LP T.Text Double
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
                mapM_ (\(i, r) -> equal' ("node-" <> show n <> "-ratio") (r *& (T.pack . show) n) (1 *& ((T.pack . show) n <> "-" <> i))) is
                mapM_ (\(item, targetN) -> equal' "link" (1 *& ((T.pack . show) n <> "-" <> item)) ((-1) *& ((T.pack . show) targetN <> "-" <> item))) links

testingGraph :: ProductionGraph
testingGraph = G.mkGraph nodes edges
  where
    nodes = [ (0, ProductionNode miner Auto ())
            , (1, ProductionNode furnace (Target 10) ())
            ]
    edges = [(0, 1, "iron-ore")]

miner = mkRecipe "miner" 1 [("iron-ore", 1)]
furnace = mkRecipe "furnace" 1 [("iron-ore", (-1)), ("iron-plate", 2)]

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
  Lua.getfield l (-1) "diesel-locomotive"

  x <- Lua.istable l (-1)
  putStrLn $ show x

  rs <- Lua.peek l (-1)
  putStrLn . show $ ((parseMaybe parseJSON (fromJust rs)) :: Maybe Recipe)

--getRecipes l = do
--  hasNext <- Lua.next l (-1)
--  if hasNext then do
--    k <- Lua.peek l (-2)
--    v <- Lua.peek l (-1)
--    Lua.pop l 1
--    getRecipes l >>= \xs -> return $ (fromJust k, fromJust v):xs
--  else return []

--getTable :: (Lua.StackValue k, Lua.StackValue v) => Lua.LuaState -> Int -> IO [(k,v)]
--getTable lua i = do
--  Lua.pushnil lua
--  getTable' (i - 1)
--  where
--    getTable' idx = do
--      hasNext <- Lua.next lua idx
--      if hasNext then do
--        k <- Lua.peek lua (-2)
--        v <- Lua.peek lua (-1)
--        Lua.pop lua 1
--        getTable' idx >>= \tl -> return $ (fromJust k, fromJust v):tl
--        else return []
--  Lua.loadstring l "return 5"
--  x <- Lua.peek l (-1)
--  putStrLn $ show (x :: Maybe Int)
--
--  putStrLn "ok"

someFunc :: IO ()
someFunc = loadLua

someFunc2 :: IO ()
someFunc2 =
  let r = furnace
  in do putStrLn . T.unpack $ pretty (lp testingGraph)
        print =<< glpSolveVars mipDefaults (lp testingGraph)
