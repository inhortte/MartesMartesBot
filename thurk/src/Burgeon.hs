{-# LANGUAGE OverloadedStrings #-}

module Burgeon
  ( hashUm
  , evalInsult
  , templ1
  , insultTemplates
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
type TemplateTitle = String
data WordGroup = WordGroup GroupTitle [String] deriving (Show)
data TemplatePart a = Part a | Parts [TemplatePart a] deriving (Show)
data InsultTemplate a b = InsultTemplate TemplateTitle (TemplatePart WordGroup) deriving (Show)


initialWordGroups :: [WordGroup]
initialWordGroups = [WordGroup "ImpVerb" ["kill", "depilate", "ogle"], WordGroup "Article" ["the", "a"], WordGroup "AnimalsPlural" ["mustelids", "humans", "snails"]]

impVerb :: WordGroup
impVerb = initialWordGroups !! 0
article :: WordGroup
article = initialWordGroups !! 1
animalsPlural :: WordGroup
animalsPlural = initialWordGroups !! 2

templ1 :: InsultTemplate TemplateTitle (TemplatePart WordGroup)
templ1 = InsultTemplate "VerbAnimal" $ Parts [Part impVerb, Part animalsPlural]

hashUm :: String -> (B.ByteString, B.ByteString)
hashUm a = let a' = B.pack a
               hash = B.pack $ show (C.hash a' :: C.Digest C.SHA256)
           in (hash, a')

toKey :: [String] -> B.ByteString
toKey = B.pack . concat . intersperse ":" . (redisPrefix:)

evalInsult :: InsultTemplate TemplateTitle (TemplatePart WordGroup) -> IO String
evalInsult (InsultTemplate _ tp) = evalTemplate tp

evalTemplate :: TemplatePart WordGroup -> IO String
evalTemplate (Part (WordGroup _ ws)) = randomFromList ws
evalTemplate (Parts tps) = do
  parts <- forM tps $ \tp -> evalTemplate tp
  return $ concat $ intersperse " " parts

insultTemplates :: IO [TemplateTitle]
insultTemplates = do
  conn <- connect martesConnectInfo
  runRedis conn $ do
    Right titles <- smembers (toKey ["templates"])
    liftIO $ return $ map B.unpack titles
    
