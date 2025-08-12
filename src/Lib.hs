{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Lib (API, register, login, notes) where

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

type API auths =
  "auth"
    :> "register"
    :> ReqBody '[JSON] UserAuth
    :> Post
        '[JSON]
        String
    :<|> "auth"
      :> "login"
      :> ReqBody '[JSON] UserAuth
      :> Post
          '[JSON]
          ( Headers
              '[ Header "Set-Cookie" SetCookie
               , Header "Set-Cookie" SetCookie
               ]
              String
          )
    :<|> Auth auths User
      :> "notes"
      :> Get '[JSON] String

getUserByName :: String -> IO (Maybe User)
getUserByName _userName = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM users WHERE userName = ?" (Only _userName) :: IO [User]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

insertUser :: UserAuth -> IO ()
insertUser UserAuth{..} = do
  conn <- open "simple.db"
  execute conn "INSERT INTO users (userName, userPassword) VALUES (?, ?)" (userName, userPassword)
  close conn

register :: UserAuth -> Handler String
register UserAuth{..} = do
  mUser <- liftIO $ getUserByName userName
  _ <- case mUser of
    Just _ -> throwError err409{errBody = "User already exists"}
    Nothing -> liftIO $ insertUser UserAuth{..}
  return "User registered successfully"

getUserByNameAndPassword :: String -> String -> IO (Maybe User)
getUserByNameAndPassword _userName _userPassword = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM users WHERE userName = ? AND userPassword = ?" (_userName, _userPassword) :: IO [User]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

login ::
  CookieSettings ->
  JWTSettings ->
  UserAuth ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
login cookieSettings jwtSettings UserAuth{..} = do
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

notes :: AuthResult User -> Handler String
notes (Authenticated User{..}) = return $ "Hello " <> userName <> "!"
notes _ = throwError err400{errBody = "Authentication required"}
