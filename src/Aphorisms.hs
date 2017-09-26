{-# LANGUAGE OverloadedStrings #-}

module Aphorisms
  ( eligeSentenceFromBlog
  , quoteBookToLines
  , quoteBookToQuotes
  , evalLine
  , qbRegexQuote
  , qbRegexHuman
  , qbRegexDate
  , stringToQuote
  , eligeQuote
  ) where

import Control.Monad (mapM, forM)
import System.Random (randomR, randomRs, getStdGen, newStdGen)
import Data.Int
import System.FilePath.Posix
import System.Directory
import System.IO
import Text.Regex.Posix
import Data.Dates
import qualified Data.Text as T
import Data.Text.ICU.Replace (replaceAll)
import Data.List.Split (splitOn)

type Human = String
data Quote = Quote [String] [Human] DateTime deriving (Show)

blogDirs :: [FilePath]
blogDirs = ["/home/polaris/Dropbox/archiv/martenblog", "/home/polaris/Dropbox/archiv/christian"]
blogRegex :: String
blogRegex = "[A-Z][[:alpha:][:space:],;:\\\"']+[\\.\\?\\!]"

quotebookFilename :: FilePath
quotebookFilename = "/home/polaris/Dropbox/draining_the_pond/Three_Subject_Quotebook.txt"
qbRegexQuote :: String
qbRegexQuote = "\\\"[[:alnum:][:space:],;:'\\.\\?\\!\\(\\)]+\\\""
qbRegexHuman :: String
qbRegexHuman = "-[[:alpha:][:space:]]+[\\(\\\"]"
qbRegexDate :: String
qbRegexDate = "\\(([0-9]+/){0,1}([0-9]+/){0,1}([0-9]+)\\)"

stringToQuote :: String -> Quote
stringToQuote s = let quotes = map (\s -> T.unpack $ replaceAll "(\\s+)" " " (T.pack s)) $ getAllTextMatches $ s =~ qbRegexQuote :: [String]
                      humans = map (T.unpack . T.strip . T.pack . tail . init) $ getAllTextMatches $ s =~ qbRegexHuman :: [String]
                      dateArr :: [String]
                      dateArr = splitOn "/" . tail . init . head . getAllTextMatches $ s =~ qbRegexDate :: [String]
                      date = case length dateArr of
                               1 -> DateTime (read (dateArr !! 0) :: Int) 1 1 0 0 0
                               2 -> DateTime (read (dateArr !! 1) :: Int) (read (dateArr !! 0) :: Int) 1 0 0 0
                               _ -> DateTime (read (dateArr !! 2) :: Int) (read (dateArr !! 0) :: Int) (read (dateArr !! 1) :: Int) 0 0 0
                  in Quote quotes humans date

firstFiveQuotes :: [Quote] -> IO ()
firstFiveQuotes qs = mapM_ (putStrLn . show) qs

evalLine :: String -> [String] -> [String]
evalLine line [] = line:[]
evalLine line quotes@(q:qs) | (T.null . T.strip . T.pack) line  = "":quotes
                            | otherwise                         = (line ++ " " ++ q):qs

splitIntoQuotes :: [String] -> IO [String]
splitIntoQuotes ls = return $ foldr evalLine [] ls
          
quoteBookToQuotes :: IO ()
quoteBookToQuotes = readFile quotebookFilename >>= (\contents -> return $ lines contents) >>= splitIntoQuotes >>= (\quotes -> return $ take 5 $ map stringToQuote quotes) >>= firstFiveQuotes

quoteBookToQuotesIO :: IO [Quote]
quoteBookToQuotesIO = readFile quotebookFilename >>= (\contents -> return $ lines contents) >>= splitIntoQuotes >>= (\quotes -> return $ map stringToQuote quotes)

quoteBookToLines :: IO ()
quoteBookToLines = readFile quotebookFilename >>= (\contents -> return $ lines contents) >>= splitIntoQuotes >>= (\quotes -> return $ take 5 quotes) >>= (\qs -> mapM_ putStrLn qs)

quoteBook :: IO [Quote]
quoteBook = quoteBookToQuotesIO

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

quoteToString :: Quote -> String
quoteToString (Quote qs hs d) = (foldr scruntch "" (zip qs hs)) ++ fecha d
  where scruntch (quote, human) gunge = quote ++ " " ++ "-" ++ human ++ "... " ++ gunge
        fecha d = (show $ year d) ++ "-" ++ (show $ month d) ++ "-" ++ (show $ day d)
                                         
eligeQuote :: IO String
eligeQuote = do
  quoteBook >>= randomFromList >>= (\q -> return $ quoteToString q)
  
  
  
