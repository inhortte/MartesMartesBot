{-# LANGUAGE OverloadedStrings #-}

module Burgeon
  ( hashAphorisms
  ) where

import qualified Crypto.Hash.SHA256 as C
import qualified Data.ByteString.Char8 as B
import Database.Redis

redisPrefix :: String
redisPrefix = "martesBot"

-- conn :: Connection


hashAphorisms :: [String] -> IO ()
hashAphorisms as = do
  let hashes :: [(B.ByteString, String)]
      hashes = map (\a -> ((C.hash . B.pack) a, a)) as
  putStrLn $ show hashes
