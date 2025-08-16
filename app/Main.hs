{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server

import API
import DB
import Server

port :: Int
port = 8080

main :: IO ()
main = do
  createDb

  putStrLn ("Server running on port " ++ show port)

  let cookieSettings = defaultCookieSettings

  jwtSecretKey <- generateKey
  let jwtSettings = defaultJWTSettings jwtSecretKey

  let config = cookieSettings :. jwtSettings :. EmptyContext

  run port
    $ serveWithContext
      (Proxy :: Proxy (API '[JWT, Cookie]))
      config
    $ server cookieSettings jwtSettings
