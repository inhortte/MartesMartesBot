{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Gruel
    ( startApp
    , app
    ) where

import Aphorisms (eligeSentenceFromBlog, eligeQuote, eligeQuoteByHuman)
import Burgeon (hashUm, insultTemplates)
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics hiding (from)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (replicateM)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Data.Maybe
import Data.Monoid
import Web.Telegram.API.Bot
import System.Environment
import Data.Version (showVersion, makeVersion)
import Text.Regex.Posix

data Version = Version
  { version :: Text
  } deriving (Show, Generic)

instance ToJSON Version

type BotAPI = "version" :> Get '[JSON] Version
              :<|> "goat"
              :> Capture "secret" Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()

botApi :: Proxy BotAPI
botApi = Proxy

startApp :: IO ()
startApp = do
  putStrLn "Martes Martes Bot is considering altering the universe"  
  manager' <- newManager tlsManagerSettings
  let telegramToken' = "349544027:AAFCoyJUK3IZe3qxxSyxxk7TqcQpfvuUSGA"
      config = BotConfig
        { telegramToken = Token $ T.pack $ "bot" <> telegramToken'
        , manager = manager'
        }
  run 9091 $ app config

newtype Bot a = Bot
    { runBot :: ReaderT BotConfig Handler a
    } deriving ( Functor, Applicative, Monad, MonadIO, -- classes from base and transformers
                 MonadReader BotConfig, MonadError ServantErr) -- classes from mtl for

data BotConfig = BotConfig
  { telegramToken :: Token
  , manager :: Manager
  }

app :: BotConfig -> Application
app config = serve botApi $ initBotServer config

initBotServer :: BotConfig -> Server BotAPI
initBotServer config = enter (transform config) botServer
    where transform :: BotConfig -> Bot :~> Handler 
          transform config = NT (flip runReaderT config . runBot)

-- actual server implementation
botServer :: ServerT BotAPI Bot
botServer = returnVersion :<|> handleWebhook
    where version' = Version $ T.pack $ showVersion $ makeVersion [1,2,3]
          returnVersion :: Bot Version
          returnVersion = return version'
          handleWebhook :: Text -> Update -> Bot ()
          handleWebhook secret update = do
              Token token <- asks telegramToken
              if EQ == compare secret token
                 then handleUpdate update
                 else throwError err403

handleUpdate :: Update -> Bot ()
handleUpdate update = do
    case update of
      Update { message = Just msg } -> handleMessage msg
      Update { inline_query = Just iq } -> handleInlineQuery iq
      _ -> liftIO $ putStrLn $ "Handle update failed. " ++ show update
--        Update { message = Just Message
--          { successful_payment = Just payment } } -> handleSuccessfulPayment payment
--        Update { message = Just msg } -> handleMessage msg
--        Update { ... } more cases
--        Update { pre_checkout_query = Just query } -> handlePreCheckout query


handleInlineQuery :: InlineQuery -> Bot ()
handleInlineQuery iq = do
  let cmdArgs = parseInlineQuery $ query_query iq
      iqId = query_id iq

      onCommand = case cmdArgs of
                    Just (cmd,args) | cmd == "goat" -> inlineAphorisms iqId args False
                                    | cmd == "koza" -> inlineAphorisms iqId args True
                                    | cmd == "qb" || cmd == "quote" || cmd == "quotebook" -> inlineQuote eligeQuote iqId args
                                    | cmd == "qsearch" -> inlineQuoteSearch eligeQuoteByHuman iqId args
                                    | cmd == "its" || cmd == "itemplates" || cmd == "insulttemplates" -> dishITemplates iqId args
                                    | otherwise -> return ()
                    Nothing -> return ()
  -- liftIO $ putStrLn $ "Inline query -> " ++ (show iq)
  onCommand

parseInlineQuery :: Text -> Maybe (String,[String])
parseInlineQuery textQuery = do
  let stringQuery = (T.unpack textQuery) :: String
      re = "\\w+" :: String
      argsList = getAllTextMatches $ (stringQuery =~ re :: AllTextMatches [] String)
  if null argsList then Nothing else Just (head argsList, tail argsList)

dishITemplates :: Text -> [String] -> Bot ()
dishITemplates iqId args = do
  _ <- liftIO $ putStrLn $ "args: " ++ show args
  BotConfig{..} <- ask
  tNames <- liftIO insultTemplates
  _ <- liftIO $ putStrLn $ show tNames
  let tNames' = if null tNames then ["Nothing"] else tNames
      buttons = map (\tName ->
                       InlineKeyboardButton (T.pack tName) Nothing (Just $ "template#" ++ tName) Nothing Nothing Nothing Nothing)
                tNames'
      keyboard = InlineKeyboardMarkup [buttons]
      inlineQueryResults = [ InlineQueryResultArticle (T.pack $ "templateresults") (Just $ T.pack $ "Click this, dead one") (Just $ InputTextMessageContent (T.pack "You are dead") Nothing Nothing) (Just keyboard) Nothing Nothing (Just $ T.pack "You have died") Nothing Nothing Nothing ]
      request = AnswerInlineQueryRequest iqId inlineQueryResults (Just 1) Nothing Nothing Nothing Nothing
  res <- ($) liftIO $ answerInlineQuery telegramToken request manager
  _ <- liftIO $ putStrLn $ show res
  case res of
    Left e -> do
      _ <- liftIO $ putStrLn $ "Error: " ++ (show e)
      return ()
    Right r -> do
      _ <- liftIO $ putStrLn $ "Response: " ++ (show r)
      return ()

inlineAphorisms :: Text -> [String] -> Bool -> Bot ()
inlineAphorisms iqId args askToClassify = do
  BotConfig{..} <- ask
  let n = if null args then 5 else read $ head args
  sentences <- liftIO $ replicateM n eligeSentenceFromBlog

  -- _ <- liftIO $ putStrLn (show sentences)
  -- _ <- liftIO $ hashUm sentences
  
  let classifyButton = InlineKeyboardButton (T.pack "classify aphorism") Nothing (Just "classify") Nothing Nothing Nothing Nothing
      keyboard = InlineKeyboardMarkup [ [classifyButton] ]
      inlineQueryResults = map (\(a,idx) -> InlineQueryResultArticle (T.pack $ "aphorism" ++ show idx) (Just $ T.pack $ "aphorism #" ++ show idx) (Just $ InputTextMessageContent (T.pack a) Nothing Nothing) (if askToClassify then (Just keyboard) else Nothing) Nothing Nothing (Just $ T.pack a) Nothing Nothing Nothing) (zip sentences [1..n])
      request = AnswerInlineQueryRequest iqId inlineQueryResults (Just 1) Nothing Nothing Nothing Nothing
  res <- ($) liftIO $ answerInlineQuery telegramToken request manager
  case res of
    Left e -> do
      _ <- liftIO $ putStrLn $ "Error: " ++ (show e)
      return ()
    Right r -> do
      _ <- liftIO $ putStrLn $ "Response: " ++ (show r)
      return ()

inlineQuoteSearch :: (String -> IO String) -> Text -> [String] -> Bot ()
inlineQuoteSearch qFn iqId args = if null args then return () else inlineQuote (qFn $ head args) iqId (tail args)

inlineQuote :: IO String -> Text -> [String] -> Bot ()
inlineQuote qFn iqId args = do
  BotConfig{..} <- ask
  let n = if null args then 3 else (read . head) args
  quotes <- liftIO $ replicateM n qFn
  let inlineQueryResults = map (\(a,idx) -> InlineQueryResultArticle (T.pack $ "quote" ++ show idx) (Just $ T.pack $ "quote #" ++ show idx) (Just $ InputTextMessageContent (T.pack a) Nothing Nothing) Nothing Nothing Nothing (Just $ T.pack a) Nothing Nothing Nothing) (zip quotes [1..n])
      request = AnswerInlineQueryRequest iqId inlineQueryResults (Just 1) Nothing Nothing Nothing Nothing
  res <- ($) liftIO $ answerInlineQuery telegramToken request manager
  case res of
    Left e -> do
      _ <- liftIO $ putStrLn $ "Error: " ++ (show e)
      return ()
    Right r -> do
      _ <- liftIO $ putStrLn $ "Response: " ++ (show r)
      return ()

handleMessage :: Message -> Bot ()
handleMessage msg = do
  BotConfig{..} <- ask
  let chatId = ChatId $ chat_id $ chat msg
      Just messageText = text msg

      onCommand (T.stripPrefix "/help" -> Just _) = sendHelpMessage chatId
      onCommand (T.stripPrefix "/goat" -> Just _) = sendAphorism chatId
      onCommand _ = sendHelpMessage chatId
      
  liftIO $ putStrLn $ "Message id -> " ++ (show $ message_id msg)
  liftIO $ putStrLn $ "Message text -> " ++ (T.unpack messageText)
  liftIO $ putStrLn $ "Chat id -> " ++ (show $ chat_id $ chat msg)

  onCommand messageText

helpMessage userId = sendMessageRequest userId $ T.unlines
    [ "/help - show this message and subsequently die the flame death"
    , "/goat - get a random phrase"
    ]

sendHelpMessage :: ChatId -> Bot ()
sendHelpMessage chatId = do
  BotConfig{..} <- ask
  liftIO $ sendMessage telegramToken (helpMessage chatId) manager >> return ()
  return ()  

sendAphorism :: ChatId -> Bot ()
sendAphorism chatId = do
  BotConfig{..} <- ask
  sentence <- liftIO eligeSentenceFromBlog
  let sendDeityMessageRequest = sendMessageRequest chatId (T.pack sentence)
  _ <- ($) liftIO $ sendMessage telegramToken sendDeityMessageRequest manager
  return ()


