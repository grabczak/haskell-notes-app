{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (close, execute_, open)
import Lib (API, login, notes, register)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings = register :<|> login cookieSettings jwtSettings :<|> notes

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
