{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib (runServant) where

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

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position -- /position/1/2
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
    :<|> "name" :> Get '[HTML] H.Html
    :<|> "age" :> Get '[HTML] H.Html
    :<|> "name" :> ReqBody '[JSON] NameWrapper :> Post '[PlainText] String

server :: Server API
server =
  positionHandler
    :<|> helloHandler
    :<|> marketingHandler
    :<|> nameHandler
    :<|> ageHandler
    :<|> namePostHandler

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runServant :: IO ()
runServant = run 5000 app
