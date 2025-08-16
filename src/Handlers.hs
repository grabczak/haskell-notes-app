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

register :: Login -> Handler User
register Login{..} = do
  dbUser <- liftIO $ selectUserByName userName
  case dbUser of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> do
      userId <- liftIO $ insertUser Login{..}
      user <- liftIO $ selectUserById userId
      case user of
        Nothing -> throwError err500{errBody = "User creation failed"}
        Just userData -> return userData

login ::
  CookieSettings ->
  JWTSettings ->
  Login ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
login cookieSettings jwtSettings Login{..} = do
  dbUser <- liftIO $ selectUserByName userName
  case dbUser of
    Nothing -> throwError $ err401{errBody = "User not found"}
    Just mUser@User{userId} -> do
      mPassword <- liftIO $ selectUserPasswordById userId
      case mPassword of
        Nothing -> throwError $ err401{errBody = "Invalid password"}
        Just hashedPassword -> do
          isValid <- liftIO $ verify userPassword (T.pack hashedPassword)
          if not isValid
            then throwError $ err401{errBody = "Invalid password"}
            else do
              loginAccepted <- liftIO $ acceptLogin cookieSettings jwtSettings mUser
              case loginAccepted of
                Nothing -> throwError $ err401{errBody = "Login failed"}
                Just x -> do
                  jwt <- liftIO $ makeJWT mUser jwtSettings Nothing
                  case jwt of
                    Left _ -> throwError $ err401{errBody = "JWT creation failed"}
                    Right r -> return $ x (BSC.unpack r)

userGet :: AuthResult User -> Handler User
userGet (Authenticated User{..}) = do
  dbUser <- liftIO $ selectUserById userId
  case dbUser of
    Nothing -> throwError err404{errBody = "User not found"}
    Just user -> return user
userGet _ = do throwError err400{errBody = "Authentication required"}

userPut :: AuthResult User -> Login -> Handler User
userPut (Authenticated User{userId}) Login{userName, userPassword} = do
  liftIO $ updateUserById userId Login{userName, userPassword}
  dbUser <- liftIO $ selectUserById userId
  case dbUser of
    Nothing -> throwError err404{errBody = "User not found"}
    Just user -> return user
userPut _ _ = throwError err400{errBody = "Authentication required"}

notesGet :: AuthResult User -> Handler [Note]
notesGet (Authenticated User{userId}) = do
  liftIO $ selectNotesByUserId userId
notesGet _ = throwError err400{errBody = "Authentication required"}

notePost :: AuthResult User -> NewNote -> Handler Note
notePost (Authenticated User{userId}) note = do
  noteId <- liftIO $ insertNote userId note
  dbNote <- liftIO $ selectNoteById noteId
  case dbNote of
    Nothing -> throwError err500{errBody = "Note creation failed"}
    Just noteData -> return noteData
notePost _ _ = throwError err400{errBody = "Authentication required"}

noteGet :: AuthResult User -> Int -> Handler Note
noteGet (Authenticated User{userId}) noteId = do
  dbNote <- liftIO $ selectNoteById noteId
  case dbNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just note@Note{userId = ownerId} ->
      if userId /= ownerId
        then throwError err403{errBody = "Forbidden"}
        else return note
noteGet _ _ = throwError err400{errBody = "Authentication required"}

notePut :: AuthResult User -> Int -> NewNote -> Handler Note
notePut (Authenticated User{userId}) noteId note = do
  dbNote <- liftIO $ selectNoteById noteId
  case dbNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just Note{userId = ownerId} ->
      if userId /= ownerId
        then throwError err403{errBody = "Forbidden"}
        else do
          liftIO $ updateNoteById noteId note
          updatedNote <- liftIO $ selectNoteById noteId
          case updatedNote of
            Nothing -> throwError err500{errBody = "Note update failed"}
            Just mNote -> return mNote
notePut _ _ _ = throwError err400{errBody = "Authentication required"}

noteDelete :: AuthResult User -> Int -> Handler NoContent
noteDelete (Authenticated User{userId}) noteId = do
  mNote <- liftIO $ selectNoteById noteId
  case mNote of
    Nothing -> throwError err404{errBody = "Note not found"}
    Just Note{userId = ownerId} ->
      if userId /= ownerId
        then throwError err403{errBody = "Forbidden"}
        else do
          liftIO $ deleteNoteById noteId
          return NoContent
noteDelete _ _ = throwError err400{errBody = "Authentication required"}
