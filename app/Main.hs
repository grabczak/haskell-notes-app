{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import Database.SQLite.Simple hiding ((:.))
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server

data User = User
  { userId :: Int
  , userName :: String
  }
  deriving (Eq, Show, Generic, FromRow, FromJWT, FromJSON, ToJWT, ToJSON)

getUser :: Int -> IO (Maybe User)
getUser _userId = do
  conn <- open "simple.db"
  res <- query conn "SELECT * FROM users WHERE userId = ?" (Only _userId) :: IO [User]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

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

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings = registerHandler :<|> loginHandler cookieSettings jwtSettings :<|> notesHandler

loginHandler ::
  CookieSettings ->
  JWTSettings ->
  User ->
  Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] String)
loginHandler cookieSettings jwtSettings User{..} = do
  mUser <- liftIO $ getUser userId
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

notesHandler :: AuthResult User -> Handler String
notesHandler (Authenticated User{..}) = return $ "Hello " <> userName <> "!"
notesHandler _ = throwError err400{errBody = "Authentication required"}

registerHandler :: User -> Handler String
registerHandler User{..} = do
  liftIO $ do
    conn <- open "simple.db"
    execute conn "INSERT INTO users (userId, userName) VALUES (?, ?)" (userId, userName)
    close conn
  return "User registered successfully"

main :: IO ()
main = do
  conn <- open "simple.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS users (userId INTEGER PRIMARY KEY, userName TEXT NOT NULL)"
  close conn

  putStrLn "Server running on http://localhost:8080"

  jwtSecretKey <- generateKey
  let jwtSettings = defaultJWTSettings jwtSecretKey
  let cookieSettings = defaultCookieSettings
  let config = cookieSettings :. jwtSettings :. EmptyContext

  run 8080
    $ serveWithContext
      (Proxy :: Proxy (API '[JWT, Cookie]))
      config
    $ server cookieSettings jwtSettings
