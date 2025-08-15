{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Lib (API, registerHandler, loginHandler, userGetHandler, userUpdateHandler, notePostHandler, noteUpdateHandler, noteDeleteHandler, getNotesHandler, noteGetHandler) where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import Database.SQLite.Simple hiding ((:.))
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
  , noteTags :: String
  , noteDeadline :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJSON, ToJSON)

data Note = Note
  { noteId :: Int
  , userId :: Int
  , noteTitle :: String
  , noteContent :: String
  , noteTags :: String
  , noteDeadline :: String
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

getUserByName :: String -> IO (Maybe User)
getUserByName _userName = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM users WHERE userName = ?" (Only _userName) :: IO [User]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

getUserByNameAndPassword :: String -> String -> IO (Maybe User)
getUserByNameAndPassword _userName _userPassword = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM users WHERE userName = ? AND userPassword = ?" (_userName, _userPassword) :: IO [User]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

createUser :: UserAuth -> IO ()
createUser UserAuth{..} = do
  conn <- open "simple.db"
  execute conn "INSERT INTO users (userName, userPassword) VALUES (?, ?)" (userName, userPassword)
  close conn

registerHandler :: UserAuth -> Handler String
registerHandler UserAuth{..} = do
  mUser <- liftIO $ getUserByName userName
  _ <- case mUser of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> liftIO $ createUser UserAuth{..}
  return "User registered successfully"

loginHandler ::
  CookieSettings ->
  JWTSettings ->
  UserAuth ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
loginHandler cookieSettings jwtSettings UserAuth{..} = do
  mUser <- liftIO $ getUserByNameAndPassword userName userPassword
  case mUser of
    Nothing -> throwError $ err401{errBody = "User not found"}
    Just user -> do
      mLoginAccepted <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mLoginAccepted of
        Nothing -> throwError $ err401{errBody = "Login failed"}
        Just x -> do
          eJWT <- liftIO $ makeJWT user jwtSettings Nothing
          case eJWT of
            Left _ -> throwError $ err401{errBody = "JWT creation failed"}
            Right r -> return $ x (BSC.unpack r)

getUserDataById :: Int -> IO (Maybe UserData)
getUserDataById _userId = do
  conn <- open "simple.db"
  res <- query conn "SELECT userId, userName FROM users WHERE userId = ?" (Only _userId) :: IO [UserData]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

userGetHandler :: AuthResult User -> Handler UserData
userGetHandler (Authenticated User{..}) = do
  mUserData <- liftIO $ getUserDataById userId
  case mUserData of
    Nothing -> throwError err404{errBody = "User not found"}
    Just userData -> return userData
userGetHandler _ = throwError err400{errBody = "Authentication required"}

updateUserDataById :: Int -> UserAuth -> IO ()
updateUserDataById _userId UserAuth{..} = do
  conn <- open "simple.db"
  execute conn "UPDATE users SET userName = ?, userPassword = ? WHERE userId = ?" (userName, userPassword, _userId)
  close conn

userUpdateHandler :: AuthResult User -> UserAuth -> Handler String
userUpdateHandler (Authenticated User{userId}) UserAuth{userName, userPassword} = do
  liftIO $ updateUserDataById userId UserAuth{userName, userPassword}
  return "User updated successfully"
userUpdateHandler _ _ = throwError err400{errBody = "Authentication required"}

getNotesByUserId :: Int -> IO [Note]
getNotesByUserId _userId = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM notes WHERE userId = ?" (Only _userId) :: IO [Note]
  close conn
  return res

getNotesHandler :: AuthResult User -> Handler [Note]
getNotesHandler (Authenticated User{userId}) = do
  liftIO $ getNotesByUserId userId
getNotesHandler _ = throwError err400{errBody = "Authentication required"}

addNote :: Int -> NoteData -> IO ()
addNote _userId NoteData{..} = do
  conn <- open "simple.db"
  execute
    conn
    "INSERT INTO notes (userId, noteTitle, noteContent, noteTags, noteDeadline) VALUES (?, ?, ?, ?, ?)"
    (_userId, noteTitle, noteContent, noteTags, noteDeadline)
  close conn

notePostHandler :: AuthResult User -> NoteData -> Handler String
notePostHandler (Authenticated User{userId}) note = do
  liftIO $ addNote userId note
  return "Note added successfully"
notePostHandler _ _ = throwError err400{errBody = "Authentication required"}

getNoteById :: Int -> IO (Maybe Note)
getNoteById _noteId = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM notes WHERE noteId = ?" (Only _noteId) :: IO [Note]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

noteGetHandler :: AuthResult User -> Int -> Handler Note
noteGetHandler (Authenticated User{userId}) noteId = do
  mNote <- liftIO $ getNoteById noteId
  case mNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just note@Note{userId = ownerId} ->
      if userId == ownerId
        then return note
        else throwError err403{errBody = "Access denied"}
noteGetHandler _ _ = throwError err400{errBody = "Authentication required"}

updateNoteById :: Int -> NoteData -> IO ()
updateNoteById _noteId NoteData{..} = do
  conn <- open "simple.db"
  execute
    conn
    "UPDATE notes SET noteTitle = ?, noteContent = ?, noteTags = ?, noteDeadline = ? WHERE noteId = ?"
    (noteTitle, noteContent, noteTags, noteDeadline, _noteId)
  close conn

noteUpdateHandler :: AuthResult User -> Int -> NoteData -> Handler String
noteUpdateHandler (Authenticated User{userId}) noteId note = do
  mNote <- liftIO $ getNoteById noteId
  case mNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just Note{userId = ownerId} ->
      if userId == ownerId
        then return ()
        else throwError err403{errBody = "Access denied"}
  liftIO $ updateNoteById noteId note
  return "Note updated successfully"
noteUpdateHandler _ _ _ = throwError err400{errBody = "Authentication required"}

deleteNoteById :: Int -> IO ()
deleteNoteById _noteId = do
  conn <- open "simple.db"
  execute conn "DELETE FROM notes WHERE noteId = ?" (Only _noteId)
  close conn

noteDeleteHandler :: AuthResult User -> Int -> Handler String
noteDeleteHandler (Authenticated User{userId}) noteId = do
  mNote <- liftIO $ getNoteById noteId
  case mNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just Note{userId = ownerId} ->
      if userId == ownerId
        then return ()
        else throwError err403{errBody = "Access denied"}
  liftIO $ deleteNoteById noteId
  return "Note deleted successfully"
noteDeleteHandler _ _ = throwError err400{errBody = "Authentication required"}
