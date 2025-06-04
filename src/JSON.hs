{-# LANGUAGE OverloadedStrings #-}

module JSON (
    JValue(..),
    fromJSON,
    toJSON,
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific, signed, charLiteral)
import Data.List (intercalate)
import Data.Void
import Data.Text (Text)
import Data.Scientific (Scientific)

type Parser = Parsec Void Text

data JValue
  = JNull
  | JBool Bool
  | JNumber Scientific
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq)

instance Show JValue where
  show value = case value of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JNumber n -> show n
    JString s -> show s
    JArray arr -> "[" ++ intercalate ", " (map show arr) ++ "]"
    JObject obj -> "{" ++ intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ show v) obj) ++ "}"

jsonValue :: Parser JValue
jsonValue = choice
  [ jsonNull
  , jsonBool
  , jsonNumber
  , jsonString
  , jsonArray
  , jsonObject
  ]

jsonNull :: Parser JValue
jsonNull = do
  _ <- string "null"
  pure JNull

jsonBool :: Parser JValue
jsonBool = do
  b <- choice [string "true", string "false"]
  pure $ JBool (b == "true")

jsonNumber :: Parser JValue
jsonNumber = do
  n <- signed (pure ()) scientific
  pure $ JNumber n

jsonString :: Parser JValue
jsonString = do
  _ <- char '"'
  content <- manyTill charLiteral (char '"')
  pure $ JString content

jsonArray :: Parser JValue
jsonArray = do
  _ <- char '['
  values <- jsonValue `sepBy` char ','
  _ <- char ']'
  pure $ JArray values

jsonPair :: Parser (String, JValue)
jsonPair = do
  (JString key) <- jsonString
  _ <- char ':'
  value <- jsonValue
  pure (key, value)

jsonObject :: Parser JValue
jsonObject = do
  _ <- char '{'
  pairs <- jsonPair `sepBy` char ','
  _ <- char '}'
  pure $ JObject pairs

fromJSON :: Parser JValue
fromJSON = do
  value <- jsonValue
  eof
  pure value

toJSON :: JValue -> String
toJSON JNull = "null"
toJSON (JBool True) = "true"
toJSON (JBool False) = "false"
toJSON (JNumber n) = show n
toJSON (JString s) = "\"" ++ s ++ "\""
toJSON (JArray arr) = "[" ++ intercalate "," (map toJSON arr) ++ "]"
toJSON (JObject obj) = "{" ++ intercalate "," (map (\(k, v) -> "\"" ++ k ++ "\":" ++ toJSON v) obj) ++ "}"
