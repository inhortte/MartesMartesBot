{-# LANGUAGE OverloadedStrings #-}

module Burgeon
  ( hashUm,
    evalInsult,
    templ1
  ) where

import Aphorisms (randomFromList)
import Control.Monad (forM)
import qualified Crypto.Hash as C
import qualified Data.ByteString.Char8 as B
import Database.Redis
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)

redisPrefix :: String
redisPrefix = "martesBot"

martesConnectInfo :: ConnectInfo
martesConnectInfo = defaultConnectInfo { connectDatabase = 7 }

type GroupTitle = String
data WordGroup = WordGroup GroupTitle [String] deriving (Show)
data TemplatePart a = Part a | Parts [TemplatePart a] deriving (Show)
type InsultTemplate = TemplatePart WordGroup

initialWordGroups :: [WordGroup]
initialWordGroups = [WordGroup "ImpVerb" ["kill", "depilate", "ogle"], WordGroup "Article" ["the", "a"], WordGroup "AnimalsPlural" ["mustelids", "humans", "snails"]]

impVerb :: WordGroup
impVerb = initialWordGroups !! 0
article :: WordGroup
article = initialWordGroups !! 1
animalsPlural :: WordGroup
animalsPlural = initialWordGroups !! 2

templ1 :: InsultTemplate
templ1 = Parts [Part impVerb, Part article, Part animalsPlural]

hashUm :: String -> (B.ByteString, B.ByteString)
hashUm a = let a' = B.pack a
               hash = B.pack $ show (C.hash a' :: C.Digest C.SHA256)
           in (hash, a')

toKey :: [String] -> B.ByteString
toKey = B.pack . concat . intersperse ":" . (redisPrefix:)

evalInsult :: InsultTemplate -> IO String
evalInsult (Part (WordGroup _ ws)) = randomFromList ws
evalInsult (Parts tps) = do
  parts <- forM tps $ \tp -> evalInsult tp
  return $ concat $ intersperse " " parts
