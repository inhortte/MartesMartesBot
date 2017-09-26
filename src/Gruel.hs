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

import Aphorisms (eligeSentenceFromBlog, eligeQuote)
import Burgeon (hashAphorisms)
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
                                    | cmd == "qb" || cmd == "quote" || cmd == "quotebook" -> inlineQuote iqId args
                                    | otherwise -> return ()
                    Nothing -> return ()

  liftIO $ putStrLn $ "Inline query -> " ++ (show iq)

  onCommand

parseInlineQuery :: Text -> Maybe (String,[String])
parseInlineQuery textQuery = do
  let stringQuery = (T.unpack textQuery) :: String
      re = "\\w+" :: String
      argsList = getAllTextMatches $ (stringQuery =~ re :: AllTextMatches [] String)
  if null argsList then Nothing else Just (head argsList, tail argsList)
                                  

inlineAphorisms :: Text -> [String] -> Bool -> Bot ()
inlineAphorisms iqId args askToClassify = do
  BotConfig{..} <- ask
  let n = if null args then 5 else read $ head args
  sentences <- liftIO $ replicateM n eligeSentenceFromBlog

  _ <- liftIO $ putStrLn (show sentences)
  _ <- liftIO $ hashAphorisms sentences
  
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

inlineQuote :: Text -> [String] -> Bot ()
inlineQuote iqId args = do
  BotConfig{..} <- ask
  let n = if null args then 3 else (read . head) args
  quotes <- liftIO $ replicateM n eligeQuote
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


{-
handleMessage :: Message -> Bot ()
handleMessage msg = do
    BotConfig{..} <- ask
    let chatId = ChatId $ fromIntegral $ user_id $ fromJust $ from msg
        messageText = text msg
        sendHelpMessage = sendMessageM (helpMessage chatId) >> return ()
        sendInvoices books = mapM_ sendInvoiceM $ map (buildBuyBookInvoice chatId paymentsToken) books
        byTitle title book = T.isInfixOf title $ fst book -- book title contains title
        onCommand (Just (T.stripPrefix "/help" -> Just _)) = sendHelpMessage
        onCommand (Just (T.stripPrefix "/books" -> Just _)) = sendInvoices allBooks
        onCommand (Just (T.stripPrefix "/find " -> Just title)) = sendInvoices $ filter (byTitle title) allBooks
        onCommand _ = sendHelpMessage
    liftIO $ runClient (onCommand messageText) telegramToken manager
    return ()

allBooks :: [(Text, (Text, Text, Int))]
allBooks =
  [ ("Copying and Pasting from Stack Overflow",
        ("http://i.imgur.com/fawRchq.jpg", "Cutting corners to meet arbitrary management deadlines", 7000))
  , ("Googling the Error Message",
        ("http://i.imgur.com/fhgzVEt.jpg", "The internet will make those bad words go away", 4500))
  , ("Whiteboard Interviews",
        ("http://i.imgur.com/oM9yCym.png", "Putting the candidate through the same bullshit you went through", 3200))
  , ("\"Temporary\" Workaround",
        ("http://i.imgur.com/IQBhKkT.jpg", "Who are you kidding?", 4200))
  ]

buildBuyBookInvoice (ChatId chatId) token (title, (image, description, price)) =
    (sendInvoiceRequest chatId title description payload token link code prices)
        { snd_inv_photo_url = Just image }
        where code = CurrencyCode "USD"
              payload = "book_payment_payload"
              link = "deep_link"
              prices = [ LabeledPrice title price
                       , LabeledPrice "Donation to a kitten hospital" 300
                       , LabeledPrice "Discount for donation" (-300) ]

handlePreCheckout :: PreCheckoutQuery -> Bot ()
handlePreCheckout query = do
    BotConfig{..} <- ask
    let chatId = ChatId $ fromIntegral $ user_id $ pre_che_from query
        queryId = pre_che_id query
        okRequest = AnswerPreCheckoutQueryRequest queryId True Nothing
    liftIO $ runClient (answerPreCheckoutQueryM okRequest) telegramToken manager
    return ()

handleSuccessfulPayment :: SuccessfulPayment -> Bot ()
handleSuccessfulPayment payment = do
    let totalAmount = T.pack $ show $ (suc_pmnt_total_amount payment) `div` 100
        CurrencyCode code = suc_pmnt_currency payment
    liftIO $ print $ "We have earned " <> code <> totalAmount <> ". Shipping book to the client!"
    return ()

-}
