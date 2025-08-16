{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module API (
  API,
  User (..),
  Login (..),
  Note (..),
  NewNote (..),
) where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics
import Servant
import Servant.Auth.Server

data User = User
  { userId :: Int
  , userName :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJSON, ToJSON, FromJWT, ToJWT)

data Login = Login
  { userName :: String
  , userPassword :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJSON, ToJSON)

data Note = Note
  { noteId :: Int
  , userId :: Int
  , noteTitle :: String
  , noteContent :: String
  , noteDeadline :: Int -- POSIXTime, stored as an integer in SQLite
  , noteDone :: Int -- 0 for not done, 1 for done, SQLite has its limitations with booleans
  , noteTags :: String -- "["red", "green", "blue"]", SQLite does not support arrays, so we use a string representation
  }
  deriving (Eq, Show, Generic, FromRow, FromJSON, ToJSON)

data NewNote = NewNote
  { noteTitle :: String
  , noteContent :: String
  , noteDeadline :: Int
  , noteDone :: Int
  , noteTags :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJSON, ToJSON)

type API auths =
  "auth" :> "register" :> ReqBody '[JSON] Login :> PostCreated '[JSON] User
    :<|> "auth" :> "login" :> ReqBody '[JSON] Login :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
    :<|> Auth auths User :> "user" :> "me" :> Get '[JSON] User
    :<|> Auth auths User :> "user" :> "me" :> ReqBody '[JSON] Login :> Put '[JSON] User
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Get '[JSON] [Note]
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> ReqBody '[JSON] NewNote :> PostCreated '[JSON] Note
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Capture "noteId" Int :> Get '[JSON] Note
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Capture "noteId" Int :> ReqBody '[JSON] NewNote :> Put '[JSON] Note
    -- DeleteNoContent doesn't really work with cookies, so we use NoContent
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Capture "noteId" Int :> Verb 'DELETE 204 '[JSON] NoContent
