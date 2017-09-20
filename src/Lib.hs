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
    , app
    ) where

-- import Data.Aeson
import Data.Aeson.Compat
import Data.Aeson.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics hiding (from)
import Control.Monad.Reader
import Control.Monad.Except
-- import Control.Monad.IO.Class
-- import Control.Monad.Reader.Class (MonadReader)
-- import Control.Monad.Error.Class
-- import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Data.Maybe
-- import Data.Monoid
import Web.Telegram.API.Bot
-- import System.Environment
-- import Data.Version (Version, makeVersion, showVersion)

-- MartesMartesBot

data BotConfig = BotConfig
  { telegramToken :: Token
  , manager :: Manager
  }

newtype Bot a = Bot
  { runBot :: ReaderT BotConfig Handler a
  } deriving(Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadError ServantErr)

tokenStr :: String
tokenStr = "bot349544027:AAFCoyJUK3IZe3qxxSyxxk7TqcQpfvuUSGA"
token :: Token
token = Token $ T.pack "bot349544027:AAFCoyJUK3IZe3qxxSyxxk7TqcQpfvuUSGA"
webHook :: Text
webHook = T.pack "https://martesmartesbot.zivter.net/goat"

type MartesApiDef = "goat" :> Capture "token" Text :> ReqBody '[JSON] Update :> Post '[JSON] ()
martesAPI :: Proxy MartesApiDef
martesAPI = Proxy

martesServer :: ServerT MartesApiDef (ReaderT BotConfig Handler)
martesServer = thurk
  where thurk :: Text -> Update -> ReaderT BotConfig Handler ()
        thurk incomingToken update = do
          BotConfig{..} <- ask
          let request = sendMessageRequest (ChatId 399075235) "A deity demands attention"
          res <- liftIO (sendMessage telegramToken request manager)
          liftIO $ putStrLn "Everything is cheese"
          return ()

initBotServer :: BotConfig -> Server MartesApiDef
initBotServer botConfig = enter (transform botConfig) martesServer
  where transform :: BotConfig -> ReaderT BotConfig Handler :~> ExceptT ServantErr IO
        transform botConfig = NT (flip runReaderT botConfig)

        {-
transform :: BotConfig -> Bot :~> Handler
        transform botConfig = NT (flip runReaderT botConfig . runBot)
-}

app :: BotConfig -> Application
app botConfig = serve martesAPI $ initBotServer botConfig

startApp :: IO ()
startApp = do
  putStrLn "Martes Martes Bot is considering altering the universe"
  manager' <- newManager tlsManagerSettings
  let botConfig = BotConfig
        { telegramToken = token
        , manager = manager'
        }
  result <- runClient ( do
                          info <- getWebhookInfoM
                          let request = setWebhookRequest' webHook
                          isSet <- setWebhookM request
                          getMeM ) token manager'
  print result
  run 9091 $ app botConfig

{-
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
-}
