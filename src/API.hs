{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module API (
  API,
  User (..),
  UserAuth (..),
  UserData (..),
  Note (..),
  NoteData (..),
) where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics
import Servant
import Servant.Auth.Server

data UserAuth = UserAuth
  { userName :: String
  , userPassword :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJWT, FromJSON, ToJWT, ToJSON)

data UserData = UserData
  { userId :: Int
  , userName :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJWT, FromJSON, ToJWT, ToJSON)

data User = User
  { userId :: Int
  , userName :: String
  , userPassword :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJWT, FromJSON, ToJWT, ToJSON)

data NoteData = NoteData
  { noteTitle :: String
  , noteContent :: String
  , noteDeadline :: Int
  , noteDone :: Int
  , noteTags :: String
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

type API auths =
  "auth" :> "register" :> ReqBody '[JSON] UserAuth :> Post '[JSON] String
    :<|> "auth" :> "login" :> ReqBody '[JSON] UserAuth :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
    :<|> Auth auths User :> "user" :> "me" :> Get '[JSON] UserData
    :<|> Auth auths User :> "user" :> "me" :> ReqBody '[JSON] UserAuth :> Put '[JSON] String
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Get '[JSON] [Note]
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> ReqBody '[JSON] NoteData :> Post '[JSON] String
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Capture "noteId" Int :> Get '[JSON] Note
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Capture "noteId" Int :> ReqBody '[JSON] NoteData :> Put '[JSON] String
    :<|> Auth auths User :> "user" :> "me" :> "notes" :> Capture "noteId" Int :> Delete '[JSON] String
