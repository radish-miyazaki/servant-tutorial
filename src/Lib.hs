{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib (runServant) where

import Data.Aeson (ToJSON)
import Data.Time.Calendar
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Get,
    JSON,
    Proxy (..),
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

issac :: User
issac = User "Isaac Newton" 372 "issac@netton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users :: [User]
users = [issac, albert]

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "issac" :> Get '[JSON] User

server :: Server UserAPI
server = return users :<|> return albert :<|> return issac

userAPI :: Proxy UserAPI
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

runServant :: IO ()
runServant = run 5000 app1
