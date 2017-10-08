{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Savant
  ( savantApp
  ) where

import Data.Aeson.Compat
import Data.Aeson.Types
import Servant
import GHC.Generics hiding (from)
import Network.Wai
import Network.Wai.Handler.Warp

type MartenApiDef = "martens" :> Get '[JSON] [Marten]
  :<|> "marten" :> Capture "name" String :> Get '[JSON] (Maybe Marten)
  :<|> "rinaldo" :> Get '[JSON] Marten
  :<|> "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
  :<|> "greeting" :> QueryParam "name" String :> Get '[JSON] Greeting
  :<|> "hovno" :> ReqBody '[JSON] Marten :> Post '[JSON] Email

-- Position definition
data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Show, Generic)
instance ToJSON Position

-- Greeting definition
newtype Greeting = Greeting { msg :: String } deriving (Generic)
instance ToJSON Greeting

-- The marten definition
data Temperament = Calm | Nervous | Violent | Deranged deriving (Show, Eq, Generic)
instance FromJSON Temperament
instance ToJSON Temperament
data Tipo = Americana | Foina | Martes | Pennanti | Flavigula deriving (Show, Eq, Generic)
instance FromJSON Tipo
instance ToJSON Tipo
data Marten = Marten
  { name :: String
  , email :: String
  , tipo :: Tipo
  , age :: Int
  , temperament :: Temperament
  } deriving (Show, Eq, Generic)
instance FromJSON Marten
instance ToJSON Marten

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)
instance ToJSON Email

emailForMarten :: Marten -> Email
emailForMarten marten = Email from' to' subject' body'
  where from' = "martesmartesbot@thinklikeamink.net"
        to' = email marten
        subject' = "Drink your zkurvený actimel"
        body' = "Hola, " ++ name marten ++ ",\n\n"
          ++ "Your fur has greyed significantly since "
          ++ "you turned " ++ show (age marten) ++ ". "
          ++ "This email is in hopes that you snuff it soon."

noMarten :: Marten
noMarten = Marten "Nada" "noone@null.net" Flavigula 10 Calm

martens1 :: [Marten]
martens1 =
  [ Marten "Gretel" "gretel@hopeforwildlife.net" Americana 14 Deranged
  , Marten "Henderson" "henderson@death.net" Pennanti 3 Calm
  , Marten "Bobbus" "inhortte@gmail.com" Flavigula 46 Nervous
  , Marten "Christian" "pod@abyss.net" Foina 40 Violent
  ]
rinaldo = Marten "Rinaldo" "rinaldo@tallinzoo.com" Foina 5 Calm
findMarten :: String -> [Marten] -> Maybe Marten
findMarten name martens = if ms == [] then Nothing else Just $ head ms
  where martenFilter (Marten name' _ _ _ _) =
          name == name'
        ms = filter martenFilter martens

server1 :: Server MartenApiDef
server1 = martens
  :<|> marten
  :<|> return rinaldo
  :<|> position
  :<|> greeting
  :<|> hovno
  where martens :: Handler [Marten]
        martens = return martens1

        marten :: String -> Handler (Maybe Marten)
        marten name = return $ findMarten name martens1

        position :: Int -> Int -> Handler Position
        position x y = return $ Position x y

        greeting :: Maybe String -> Handler Greeting
        greeting name = return . Greeting $ case name of
          Nothing -> "Your death will be anonymous."
          Just n -> "Die, " ++ n

        hovno :: Marten -> Handler Email
        hovno marten = return $ emailForMarten marten

martenAPI :: Proxy MartenApiDef
martenAPI = Proxy

app1 :: Application
app1 = serve martenAPI server1

savantApp :: IO ()
savantApp = run 9091 app1
