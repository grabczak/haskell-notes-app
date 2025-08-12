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

data User = User
  { userId :: Int
  , userName :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJWT, FromJSON, ToJWT, ToJSON)

type API auths =
  "register"
    :> ReqBody '[JSON] User
    :> Post
        '[JSON]
        String
    :<|> "login"
      :> ReqBody '[JSON] User
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

getUserById :: Int -> IO (Maybe User)
getUserById _userId = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM users WHERE userId = ?" (Only _userId) :: IO [User]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

register :: User -> Handler String
register User{..} = do
  liftIO $ do
    conn <- open "simple.db"
    execute conn "INSERT INTO users (userId, userName) VALUES (?, ?)" (userId, userName)
    close conn
  return "User registered successfully"

login ::
  CookieSettings ->
  JWTSettings ->
  User ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
login cookieSettings jwtSettings User{..} = do
  mUser <- liftIO $ getUserById userId
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
