{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers (
  register,
  login,
  userGet,
  userPut,
  notesGet,
  notePost,
  noteGet,
  notePut,
  noteDelete,
) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text as T
import Servant
import Servant.Auth.Server

import API
import DB
import Lib

register :: UserAuth -> Handler String
register UserAuth{..} = do
  mUser <- liftIO $ selectUserByName userName
  _ <- case mUser of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> liftIO $ insertUser UserAuth{..}
  return "User registered successfully"

login ::
  CookieSettings ->
  JWTSettings ->
  UserAuth ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
login cookieSettings jwtSettings UserAuth{..} = do
  mUser <- liftIO $ selectUserByName userName
  case mUser of
    Nothing -> throwError $ err401{errBody = "User not found"}
    Just user@User{userPassword = hashedPassword} -> do
      isValid <- liftIO $ verify userPassword (T.pack hashedPassword)
      if not isValid
        then throwError $ err401{errBody = "Invalid password"}
        else do
          mLoginAccepted <- liftIO $ acceptLogin cookieSettings jwtSettings user
          case mLoginAccepted of
            Nothing -> throwError $ err401{errBody = "Login failed"}
            Just x -> do
              eJWT <- liftIO $ makeJWT user jwtSettings Nothing
              case eJWT of
                Left _ -> throwError $ err401{errBody = "JWT creation failed"}
                Right r -> return $ x (BSC.unpack r)

userGet :: AuthResult User -> Handler UserData
userGet (Authenticated User{..}) = do
  mUserData <- liftIO $ selectUserById userId
  case mUserData of
    Nothing -> throwError err404{errBody = "User not found"}
    Just userData -> return userData
userGet _ = throwError err400{errBody = "Authentication required"}

userPut :: AuthResult User -> UserAuth -> Handler String
userPut (Authenticated User{userId}) UserAuth{userName, userPassword} = do
  liftIO $ updateUserById userId UserAuth{userName, userPassword}
  return "User updated successfully"
userPut _ _ = throwError err400{errBody = "Authentication required"}

notesGet :: AuthResult User -> Handler [Note]
notesGet (Authenticated User{userId}) = do
  liftIO $ selectNotesByUserId userId
notesGet _ = throwError err400{errBody = "Authentication required"}

notePost :: AuthResult User -> NoteData -> Handler String
notePost (Authenticated User{userId}) note = do
  liftIO $ insertNote userId note
  return "Note added successfully"
notePost _ _ = throwError err400{errBody = "Authentication required"}

noteGet :: AuthResult User -> Int -> Handler Note
noteGet (Authenticated User{userId}) noteId = do
  mNote <- liftIO $ selectNoteById noteId
  case mNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just note@Note{userId = ownerId} ->
      if userId == ownerId
        then return note
        else throwError err403{errBody = "Access denied"}
noteGet _ _ = throwError err400{errBody = "Authentication required"}

notePut :: AuthResult User -> Int -> NoteData -> Handler String
notePut (Authenticated User{userId}) noteId note = do
  mNote <- liftIO $ selectNoteById noteId
  case mNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just Note{userId = ownerId} ->
      if userId == ownerId
        then return ()
        else throwError err403{errBody = "Access denied"}
  liftIO $ updateNoteById noteId note
  return "Note updated successfully"
notePut _ _ _ = throwError err400{errBody = "Authentication required"}

noteDelete :: AuthResult User -> Int -> Handler String
noteDelete (Authenticated User{userId}) noteId = do
  mNote <- liftIO $ selectNoteById noteId
  case mNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just Note{userId = ownerId} ->
      if userId == ownerId
        then return ()
        else throwError err403{errBody = "Access denied"}
  liftIO $ deleteNoteById noteId
  return "Note deleted successfully"
noteDelete _ _ = throwError err400{errBody = "Authentication required"}
