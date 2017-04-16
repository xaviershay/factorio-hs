{-# LANGUAGE OverloadedStrings #-}

module Factorio.Lua
  ( loadFactorioData
  ) where

import Control.Monad    (filterM, forM_)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Maybe       (fromJust)
import Data.Monoid      ((<>))

import qualified Scripting.Lua       as Lua
import           Scripting.Lua.Aeson

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath  (FilePath, joinPath)

import Factorio.Types

-- Loads recipe information from the given base factorio path. Mods not
-- currently supported, though should be a trivial extension.
loadFactorioData :: FilePath -> IO ProductionData
loadFactorioData factorioPath = do
  l <- Lua.newstate
  e <- Lua.openlibs l
  e <- addPackagePath l packagePath
  Lua.getglobal l "package"
  Lua.getfield l (-1) "path"
  -- TODO: Crashes if file doesn't exist
  e <- Lua.loadfile l $ dataPath <> "/core/lualib/dataloader.lua"
  e <- Lua.call l 0 0
  e <- Lua.loadfile l "patch.lua"
  e <- Lua.call l 0 0
  dirs <- listDirectories dataPath
  -- TODO Reset package path
  -- TODO Clear loaded table
  forM_ dirs $ \modDir
   -> do
    addPackagePath l modDir
    e <- Lua.loadfile l (joinPath [modDir, "data.lua"])
    e <- Lua.call l 0 0
    return ()
  Lua.getglobal l "data"
  Lua.getfield l (-1) "raw"
  Lua.getfield l (-1) "recipe"
  rs <- Lua.peek l (-1)
  case parseEither parseJSON (fromJust rs) of
    Left e        -> fail e
    Right recipes -> return recipes

  where
    dataPath = joinPath [factorioPath, "data"]
    packagePath = dataPath <> "/core/lualib"
    addPackagePath l p = do
      Lua.loadstring l ("package.path = package.path .. ';" <> p <> "/?.lua'")
      Lua.call l 0 0

listDirectories :: FilePath -> IO [FilePath]
listDirectories path = do
  e <- doesDirectoryExist path
  if e
    then do
      files <- listDirectory path
      let fullDirs = map (\x -> joinPath [path, x]) files
      filterM doesDirectoryExist fullDirs
    else return []
