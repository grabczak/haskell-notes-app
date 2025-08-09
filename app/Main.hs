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
  query_,
 )
import GHC.Generics (Generic)
import Web.Scotty (get, json, liftIO, pathParam, post, scotty, text)

-- Model
data User = User Int Text deriving (Show, Generic)
instance FromRow User where
  fromRow = User <$> field <*> field
instance ToJSON User

main :: IO ()
main = do
  conn <- open "simple.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"
  close conn

  scotty 8080 $ do
    -- GET /users - list all users
    get "/users" $ do
      users <- liftIO $ do
        _conn <- open "simple.db"
        us <- query_ _conn "SELECT id, name FROM users" :: IO [User]
        close _conn
        return us
      json users

    -- POST /users/:name - insert a new user
    post "/users/:name" $ do
      name <- pathParam "name"
      liftIO $ do
        _conn <- open "simple.db"
        execute _conn "INSERT INTO users (name) VALUES (?)" (Only (name :: Text))
        close _conn
      text ("Added user: " `TL.append` name)
