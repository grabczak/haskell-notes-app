{-# LANGUAGE DataKinds #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant

-- Define the API type
type API = Get '[JSON] String

-- Implement the server
server :: Server API
server = return "Hello, World!"

-- Application
app :: Application
app = serve (Proxy :: Proxy API) server

-- Main function
main :: IO ()
main = do
  putStrLn "Running on http://localhost:8080"
  run 8080 app
