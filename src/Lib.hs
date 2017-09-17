{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib
    ( startApp
    ) where

-- import Data.Aeson
import Data.Aeson.Compat
import Data.Aeson.Types
-- import Network.Wai
-- import Network.Wai.Handler.Warp
import Servant
import GHC.Generics hiding (from)
import Control.Monad.Trans.Reader
-- import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Data.Maybe
-- import Data.Monoid
import Web.Telegram.API.Bot
-- import System.Environment
-- import Data.Version (Version, makeVersion, showVersion)

type MartenApi = "martens" :> Get '[JSON] [Marten]

data Temperament = Calm | Nervous | Violent | Deranged deriving (Show, Eq)
data Tipo = Americana | Foina | Martes | Pennanti | Flavigula deriving (Show, Eq)
data Marten = Marten
  { name :: String
  , tipo :: Tipo
  , age :: Int
  , temperament :: Temperament
  } deriving (Show, Eq, Generic)

martens1 :: [Marten]
martens1 =
  [ Marten "Gretel" Americana 14 Deranged
  , Marten "Henderson" Pennanti 3 Calm
  , Marten "Bobbus" Flavigula 46 Nervous
  , Marten "Christian" Foina 40 Violent
  ]

startApp :: IO ()
startApp = do
  manager <- newManager tlsManagerSettings
  let request = sendMessageRequest chatId  "A deity demands attention"
  res <- sendMessage token request manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right Response { result = m } -> do
      putStrLn "Request succeeded"
      print $ message_id m
      print $ text m
  where token = Token "bot349544027:AAFCoyJUK3IZe3qxxSyxxk7TqcQpfvuUSGA"
        chatId = ChatId 399075235
