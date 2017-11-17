{-# LANGUAGE OverloadedStrings #-}

module Burgeon
  ( hashUm
  , evalInsult
  , templ1
  , insultTemplates
  , insultTemplate
  , wordGroups
  , wordGroupWords
  ) where

import Aphorisms (randomFromList)
import Control.Monad (forM)
import qualified Crypto.Hash as C
import qualified Data.ByteString.Char8 as B
import Database.Redis
import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import Text.Regex.Posix

redisPrefix :: String
redisPrefix = "martesBot"

martesConnectInfo :: ConnectInfo
martesConnectInfo = defaultConnectInfo { connectHost = "127.0.0.1", connectDatabase = 7 }

type GroupTitle = String
type TemplateTitle = String
data WordGroup = WordGroup GroupTitle [String] deriving (Show)
data TemplatePart a = Part a | Parts [TemplatePart a] deriving (Show)
data InsultTemplate a b = InsultTemplate TemplateTitle (TemplatePart WordGroup) deriving (Show)
data PartType = WG | TEMPL | FIXED | BOOM

partType :: String -> (PartType, String)
partType s | (s :: String) =~ ("^WG" :: String) :: Bool = (WG, drop 2 s)
           | (s :: String) =~ ("^TEMPL" :: String) :: Bool = (TEMPL, drop 5 s)
           | (s :: String) =~ ("^FIXED" :: String) :: Bool = (FIXED, drop 5 s)
           | otherwise = (BOOM, s)

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
  conn <- checkedConnect martesConnectInfo
  runRedis conn $ do
    Right titles <- smembers (toKey ["templates"])
    liftIO $ return $ map B.unpack titles

wordGroup :: GroupTitle -> IO WordGroup
wordGroup gt = do
  conn <- checkedConnect martesConnectInfo
  runRedis conn $ do
    Right ws <- smembers (toKey ["wordgroup", gt])
    liftIO $ return $ WordGroup gt $ map B.unpack ws

wordGroupWords :: GroupTitle -> IO [String]
wordGroupWords gt = wordGroup gt >>= (\(WordGroup _ ws) -> return ws)

wordGroups :: IO [GroupTitle]
wordGroups = do
  conn <- checkedConnect martesConnectInfo
  runRedis conn $ do
    Right titles <- smembers (toKey ["wordgroups"])
    liftIO $ return $ map B.unpack titles

templatePart :: TemplateTitle -> IO (TemplatePart WordGroup)
templatePart tTitle = do
  conn <- checkedConnect martesConnectInfo
  runRedis conn $ do
    Right parts <- zrange (toKey ["template", tTitle]) 0 (-1)
    parts <- liftIO $ forM parts $ \part ->
      case partType (B.unpack part) of
        (WG, wgName) -> do
          wg <- wordGroup wgName
          return $ Part wg
        (TEMPL, tName) -> do
          tp <- templatePart tName
          return tp
        _ -> error $ "misnamed wordgroup or template: " ++ (B.unpack part)
    liftIO $ return $ Parts parts
    
insultTemplate :: TemplateTitle -> IO (InsultTemplate TemplateTitle (TemplatePart WordGroup))
insultTemplate tTitle = do
  tp <- templatePart tTitle
  return $ InsultTemplate tTitle tp
    
-- newWordGroup - create new word group
-- addWordToGroup

