{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)
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

instance Show JValue where
  show value = case value of
    JNull -> "null"
    JBool True -> "true"
    JBool False -> "false"
    JNumber n -> show n
    JString s -> show s
    JArray arr -> "[" ++ intercalate ", " (map show arr) ++ "]"
    JObject obj -> "{" ++ intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ show v) obj) ++ "}"

example :: JValue
example = JObject
  [ ("name", JString "Alice")
  , ("age", JNumber 30)
  , ("isStudent", JBool False)
  , ("courses", JArray [JString "Math", JString "Science"])
  , ("address", JObject [("city", JString "Wonderland"), ("zip", JNumber 12345)])
  ]

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
jsonNumber = do JNumber <$> scientific

jsonString :: Parser JValue
jsonString = do
  _ <- char '"'
  content <- many (noneOf ['"'])
  _ <- char '"'
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
