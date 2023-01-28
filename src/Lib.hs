{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib (runServant) where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON, parseJSON)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Capture,
    Get,
    Handler,
    JSON,
    PlainText,
    Post,
    Proxy (..),
    QueryParam,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Blaze (HTML)
import Servant.Server.Internal.ServerError
import Text.Blaze.Html5 as H
  ( Html,
    body,
    docTypeHtml,
    h1,
    head,
    title,
    toHtml,
  )

data Position = Position
  { xCoord :: Int,
    yCoord :: Int
  }
  deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String} deriving (Generic)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String,
    clientEmail :: String,
    clientAge :: Int,
    clientInterestedIn :: [String]
  }
  deriving (Generic)

instance FromJSON ClientInfo

data Email = Email
  { from :: String,
    to :: String,
    subject :: String,
    body :: String
  }
  deriving (Generic)

instance ToJSON Email

newtype NameWrapper = NameWrapper {getName :: String} deriving (Generic)

instance FromJSON NameWrapper where
  parseJSON v = NameWrapper <$> parseJSON v

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "nomadori@example.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
      "Hi "
        ++ clientName c
        ++ ".\n\n"
        ++ "Since you've recently turned "
        ++ show (clientAge c)
        ++ ", have you checked out our latest "
        ++ intercalate ", " (clientInterestedIn c)
        ++ " product? Give us a visit!"

-- Handlers
helloHandler :: Maybe String -> Handler HelloMessage
helloHandler mname = return . HelloMessage $ case mname of
  Nothing -> "Hello, anonymous coward"
  Just n -> "Hello, " ++ n

positionHandler :: Int -> Int -> Handler Position
positionHandler x y = return $ Position x y

marketingHandler :: ClientInfo -> Handler Email
marketingHandler c = return $ emailForClient c

nameHandler :: Handler H.Html
nameHandler = return . docTypeHtml $ do
  H.head $ do
    H.title "This is name page"
  H.body $ do
    H.h1 "My name is Radish"

ageHandler :: Handler H.Html
ageHandler = return . docTypeHtml $ do
  H.head $ do
    H.title "This is age page"
  H.body $ do
    H.h1 . H.toHtml $ "My age is " ++ show age ++ "."
  where
    age :: Integer
    age = 28

namePostHandler :: NameWrapper -> Handler String
namePostHandler (NameWrapper name) = return name

nameReaderMonadHandler :: Reader String String
nameReaderMonadHandler = do
  r <- ask
  return $ r ++ " : sras"

ageReaderMonadHandler :: Reader String String
ageReaderMonadHandler = return "10"

readerToHandler :: Reader String x -> Handler x
readerToHandler r = return $ runReader r "reader env"

nameWithIOHandler :: String -> Handler String
nameWithIOHandler name = do
  liftIO $ print $ "input name = " ++ name -- IO Monad
  return name

exceptHandler :: Handler String
exceptHandler =
  if True
    then
      throwError $
        err500
          { errBody = "Exception in module A.B.C:55. Have a great day!"
          }
    else return "sras"

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
    :<|> "name" :> Get '[HTML] H.Html
    :<|> "age" :> Get '[HTML] H.Html
    :<|> "name" :> ReqBody '[JSON] NameWrapper :> Post '[PlainText] String
    :<|> "person" :> "name" :> Get '[PlainText] String
    :<|> "person" :> "age" :> Get '[PlainText] String
    :<|> "name" :> "log" :> ReqBody '[JSON] String :> Post '[PlainText] String
    :<|> "errname" :> Get '[PlainText] String

server :: Server API
server =
  positionHandler
    :<|> helloHandler
    :<|> marketingHandler
    :<|> nameHandler
    :<|> ageHandler
    :<|> namePostHandler
    :<|> readerToHandler nameReaderMonadHandler
    :<|> readerToHandler ageReaderMonadHandler
    :<|> nameWithIOHandler
    :<|> exceptHandler

-- This is example to use another Monad (ex.Reader) for Server
-- type API =
--   "person" :> "name" :> Get '[PlainText] String
--     :<|> "person" :> "age" :> Get '[PlainText] String
--
-- readerServer :: ServerT API (Reader String)
-- readerServer = nameReaderMonadHandler :<|> ageReaderMonadHandler
--
-- handlerServer :: ServerT API Handler
-- handlerServer = hoistServer api readerToHandler readerServer
--
-- api :: Proxy API
-- api = Proxy
--
-- app :: Application
-- app = serve api handlerServer

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runServant :: IO ()
runServant = run 5000 app
