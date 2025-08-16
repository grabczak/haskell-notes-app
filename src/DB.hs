{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB (
  createDb,
  insertUser,
  selectUserByName,
  selectUserById,
  updateUserById,
  selectNotesByUserId,
  insertNote,
  selectNoteById,
  updateNoteById,
  deleteNoteById,
) where

import Database.SQLite.Simple

import API
import Lib

dbName :: String
dbName = "app.db"

createDb :: IO ()
createDb = withConnection dbName $ \conn -> do
  mapM_
    (execute_ conn)
    [ "CREATE TABLE IF NOT EXISTS users ( \
      \ userId INTEGER PRIMARY KEY, \
      \ userName TEXT NOT NULL, \
      \ userPassword TEXT NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS notes ( \
      \ noteId INTEGER PRIMARY KEY, \
      \ userId INTEGER NOT NULL, \
      \ noteTitle TEXT NOT NULL, \
      \ noteContent TEXT NOT NULL, \
      \ noteDone INTEGER NOT NULL, \
      \ noteDeadline INTEGER NOT NULL, \
      \ noteTags TEXT NOT NULL, \
      \ FOREIGN KEY(userId) REFERENCES users(userId))"
    ]

insertUser :: UserAuth -> IO ()
insertUser UserAuth{..} = do
  hashedPassword <- hash userPassword
  conn <- open dbName
  execute conn "INSERT INTO users (userName, userPassword) VALUES (?, ?)" (userName, hashedPassword)
  close conn

selectUserByName :: String -> IO (Maybe User)
selectUserByName userName = do
  conn <- open dbName
  res <- query conn "SELECT * FROM users WHERE userName = ?" (Only userName) :: IO [User]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

selectUserById :: Int -> IO (Maybe UserData)
selectUserById userId = do
  conn <- open dbName
  res <- query conn "SELECT userId, userName FROM users WHERE userId = ?" (Only userId) :: IO [UserData]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

updateUserById :: Int -> UserAuth -> IO ()
updateUserById userId UserAuth{..} = do
  conn <- open dbName
  execute conn "UPDATE users SET userName = ?, userPassword = ? WHERE userId = ?" (userName, userPassword, userId)
  close conn

selectNotesByUserId :: Int -> IO [Note]
selectNotesByUserId userId = do
  conn <- open dbName
  res <- query conn "SELECT * FROM notes WHERE userId = ?" (Only userId) :: IO [Note]
  close conn
  return res

insertNote :: Int -> NoteData -> IO ()
insertNote userId NoteData{..} = do
  conn <- open dbName
  execute
    conn
    "INSERT INTO notes (userId, noteTitle, noteContent, noteDeadline, noteDone, noteTags) VALUES (?, ?, ?, ?, ?, ?)"
    (userId, noteTitle, noteContent, noteDeadline, noteDone, noteTags)
  close conn

selectNoteById :: Int -> IO (Maybe Note)
selectNoteById noteId = do
  conn <- open dbName
  res <- query conn "SELECT * FROM notes WHERE noteId = ?" (Only noteId) :: IO [Note]
  close conn
  return $ case res of
    [] -> Nothing
    (x : _) -> Just x

updateNoteById :: Int -> NoteData -> IO ()
updateNoteById noteId NoteData{..} = do
  conn <- open dbName
  execute
    conn
    "UPDATE notes SET noteTitle = ?, noteContent = ?, noteDeadline = ?, noteDone = ?, noteTags = ? WHERE noteId = ?"
    (noteTitle, noteContent, noteDeadline, noteDone, noteTags, noteId)
  close conn

deleteNoteById :: Int -> IO ()
deleteNoteById noteId = do
  conn <- open dbName
  execute conn "DELETE FROM notes WHERE noteId = ?" (Only noteId)
  close conn
