{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.SQLite.Simple (close, execute_, open)
import Lib
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings =
  registerHandler
    :<|> loginHandler cookieSettings jwtSettings
    :<|> userGetHandler
    :<|> userUpdateHandler
    :<|> getNotesHandler
    :<|> notePostHandler
    :<|> noteGetHandler
    :<|> noteUpdateHandler
    :<|> noteDeleteHandler

main :: IO ()
main = do
  conn <- open "simple.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS users (userId INTEGER PRIMARY KEY, userName TEXT NOT NULL, userPassword TEXT NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS notes (noteId INTEGER PRIMARY KEY, userId INTEGER NOT NULL, noteTitle TEXT NOT NULL, noteContent TEXT NOT NULL, noteTags TEXT NOT NULL, noteDeadline TEXT NOT NULL, FOREIGN KEY(userId) REFERENCES users(userId))"
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
