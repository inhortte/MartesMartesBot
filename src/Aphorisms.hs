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

randomPhrase :: [String] -> IO String
randomPhrase pColl = do
  thurk <- (getStdGen >>= (\gen -> return . head . map (pColl !!) . take 1 . randomRs (0, length pColl - 1) $ gen))
  newStdGen
  return thurk

specifyPaths :: [FilePath] -> IO [FilePath]
specifyPaths ds = do
  mapM (\d -> do
           files <- listDirectory d
           return $ map (d </>) files)
    ds >>= (\fps -> return $ concat fps)

randomFromList :: [a] -> IO a
randomFromList xs = do
  item <- (getStdGen >>= (\gen -> return . (xs !!) . fst . randomR (0, length xs) $ gen))
  newStdGen
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
