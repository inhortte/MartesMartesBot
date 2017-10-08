module Utils
  ( randomFromList
  ) where

import System.Random (randomR, getStdGen, newStdGen)

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
