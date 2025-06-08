{-# LANGUAGE OverloadedStrings #-}

module JSONCodec (JValue (..), fromJSON, toJSON) where

import Data.List (intercalate)
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, Parsec, choice, manyTill, parse, sepBy)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (charLiteral, scientific, signed)

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
  show = toJSON

jsonScheme :: Parser JValue
jsonScheme =
  choice
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
  values <- jsonScheme `sepBy` char ','
  _ <- char ']'
  pure $ JArray values

jsonPair :: Parser (String, JValue)
jsonPair = do
  (JString key) <- jsonString
  _ <- char ':'
  value <- jsonScheme
  pure (key, value)

jsonObject :: Parser JValue
jsonObject = do
  _ <- char '{'
  pairs <- jsonPair `sepBy` char ','
  _ <- char '}'
  pure $ JObject pairs

jsonValue :: Parser JValue
jsonValue = do
  value <- jsonScheme
  eof
  pure value

fromJSON :: String -> Either (ParseErrorBundle Text Void) JValue
fromJSON input = parse jsonValue "" (pack input)

toJSON :: JValue -> String
toJSON input = case input of
  JNull -> "null"
  JBool True -> "true"
  JBool False -> "false"
  JNumber n -> show n
  JString s -> "\"" ++ s ++ "\""
  JArray arr -> "[" ++ intercalate "," (map toJSON arr) ++ "]"
  JObject obj -> "{" ++ intercalate "," (map (\(k, v) -> "\"" ++ k ++ "\":" ++ toJSON v) obj) ++ "}"
