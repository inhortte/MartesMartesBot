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
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Servant
-- import GHC.Generics hiding (from)
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

{-
data Vers = Vers { version :: Text } deriving (Show, Generic)
instance ToJSON Vers
-}
{-
type BotAPI = "version" :> Get '[JSON] Vers
data BotConfig = BotConfig
  { telegramToken :: Token
  , manager :: Manager
  }
-}
{-
newtype Bot a = Bot
  { runBot :: ReaderT BotConfig Handler a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadError ServantErr)
-}
{-
botApi :: Proxy BotAPI
botApi = Proxy
-}
{-
v :: Version
v = makeVersion [1,2,0,0]
-}
{-
botServer :: ServerT BotAPI Bot
botServer = returnVersion
  where version' = Vers $ T.pack $ showVersion v
        returnVersion :: Bot Vers
        returnVersion = return version'
-}
{-
initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
  where transform :: BotConfig -> Bot :~> ExceptT ServantErr IO
        transform config = flip runReaderT config . runBot
-}
{-
app :: BotConfig -> Application
app config = serve botApi $ initBotServer config
-}
{-
startApp :: IO ()
startApp = do
  putStrLn "Martes Martes, the crepuscular ghost, has arisen."
  env <- getEnvironment
  manager' <- newManager tlsManagerSettings
  let telegramToken' = "349544027:AAFCoyJUK3IZe3qxxSyxxk7TqcQpfvuUSGA"
      config = BotConfig
        { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
        , manager = manager'
        }
  run 8080 $ app config
-}
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
