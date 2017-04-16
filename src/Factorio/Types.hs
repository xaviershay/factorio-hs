{-# LANGUAGE OverloadedStrings #-}

module Factorio.Types
  ( Ingredient(..)
  , Item
  , ProductionData
  , ProductionGraph
  , ProductionNode(..)
  , RateSpec(..)
  , Recipe(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson          (FromJSON (..), Value (..), (.!=), (.:), (.:?))
import Data.Aeson.Types    (typeMismatch)
import Data.Monoid         ((<>))

import qualified Data.Graph.Inductive as G
import qualified Data.Map             as M
import qualified Data.Text            as T

type Item = T.Text

type AssemblerSpec = () -- TODO

data RateSpec
  = Auto
  | Target Double
  deriving (Show)

data ProductionNode =
  ProductionNode Recipe
                 RateSpec
                 AssemblerSpec
  deriving (Show)

type ProductionGraph = G.Gr ProductionNode Item

type ProductionData = M.Map T.Text Recipe

data Recipe = Recipe
  { name     :: T.Text
  --, duration :: Double
  , items    :: [Ingredient]
  , category :: T.Text
  } deriving (Show)

data Ingredient =
  Ingredient Item
             Double
  deriving (Show)

instance FromJSON Ingredient where
  parseJSON x =
    case x of
      Object o -> Ingredient <$> o .: "name" <*> o .: "amount"
      Array _ -> do
        (n, a) <- parseJSON x
        return $ Ingredient n a
      invalid -> typeMismatch "Ingredient" invalid

instance FromJSON Recipe where
  parseJSON (Object v) = do
    n        <- v .: "name"
    inputs   <- v .: "ingredients"
    category <- v .:? "category" .!= "crafting"
    outputs  <-     (v .: "results")
                <|> parseResult v
                <|> fail "Recipe must have one of: results, result"
    return
      Recipe
      { name = n
      --, duration = 1 -- TODO
      , items = map (\(Ingredient n a) -> Ingredient n (-a)) inputs <> outputs
      , category = category
      }

    where
      parseResult v = do
        item  <- v .: "result"
        count <- v .:? "result_count" .!= 1.0

        return [Ingredient item count]
