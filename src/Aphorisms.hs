module Aphorisms
  ( doleOut
  , randomFromList
  , sentenceFromFile
  , eligeSentenceFromBlog
  ) where

import Control.Monad (mapM, forM)
import System.Random (randomR, randomRs, getStdGen, newStdGen)
import Data.Int
import System.FilePath.Posix
import System.Directory
import System.IO
import Text.Regex.Posix

blogDirs :: [FilePath]
blogDirs = ["/home/polaris/Dropbox/archiv/martenblog", "/home/polaris/Dropbox/archiv/christian"]

blogRegex :: String
blogRegex = "[A-Z][[:alpha:][:space:],;:\\\"']+[\\.]"

specifyPaths :: [FilePath] -> IO [FilePath]
specifyPaths ds = do
  mapM (\d -> do
           files <- listDirectory d
           return $ map (d </>) files)
    ds >>= (\fps -> return $ concat fps)

randomFromList :: [a] -> IO a
randomFromList xs = do
  item <- (getStdGen >>= (\gen -> return . (\idx ->
                                              case idx of
                                                idx' | idx' >= length xs -> xs !! (length xs - 1)
                                                     | idx' < 0 -> xs !! 0
                                                     | otherwise -> xs !! idx') . fst . randomR (0, (if length xs == 0
                                                                                                     then 0
                                                                                                     else length xs - 1)) $ gen))
  _ <- newStdGen
  return item

eliminateNewlines :: String -> String
eliminateNewlines = map (\c -> if c == '\n' then ' ' else c) . unlines . drop 5 . lines

isolateSentences :: String -> [String]
isolateSentences contents = getAllTextMatches $ contents =~ blogRegex :: [String]

sentenceFromFile :: FilePath -> IO String
sentenceFromFile fp = readFile fp >>= (\contents -> return $ isolateSentences . eliminateNewlines $ contents) >>= randomFromList

eligeSentenceFromBlog :: IO String
eligeSentenceFromBlog = specifyPaths blogDirs >>= randomFromList >>= sentenceFromFile
  
doleOut :: [Int32] -> IO [String]
doleOut ims = do
  return ["thurk?", "bastard?"]
