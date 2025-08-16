{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server (server) where

import Servant
import Servant.Auth.Server

import API
import Handlers

server :: CookieSettings -> JWTSettings -> Server (API auths)
server cookieSettings jwtSettings =
  register
    :<|> login cookieSettings jwtSettings
    :<|> userGet
    :<|> userPut
    :<|> notesGet
    :<|> notePost
    :<|> noteGet
    :<|> notePut
    :<|> noteDelete
