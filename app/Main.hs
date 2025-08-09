{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (ToJSON)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple (
  FromRow (..),
  Only (Only),
  close,
  execute,
  execute_,
  field,
  open,
  query,
  query_,
 )
import GHC.Generics (Generic)
import Web.Scotty (ActionM, get, json, liftIO, pathParam, post, scotty, text)

data User = User
  { id :: Int
  , name :: Text
  , email :: Text
  , password :: Text
  }
  deriving (Show, Generic)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToJSON User

main :: IO ()
main = do
  conn <- open "simple.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT, email TEXT, password TEXT)"
  close conn

  scotty 8080 $ do
    get "/users" $ do
      users <- liftIO $ do
        _conn <- open "simple.db"
        us <- query_ _conn "SELECT id, name, email, password FROM users" :: IO [User]
        close _conn
        return us
      json users

    post "/users/:name/:email/:password" $ do
      _name <- pathParam "name" :: ActionM Text
      _email <- pathParam "email" :: ActionM Text
      _password <- pathParam "password" :: ActionM Text
      _users <- liftIO $ do
        _conn <- open "simple.db"
        _users <- query _conn "SELECT * FROM users WHERE email = ?" (Only _email) :: IO [User]
        case _users of
          [] -> do
            execute _conn "INSERT INTO users (name, email, password) VALUES (?, ?, ?)" (_name, _email, _password)
            close _conn
          _ -> do
            close _conn
        return _users
      if null _users
        then text "User created successfully."
        else text "User already exists."
