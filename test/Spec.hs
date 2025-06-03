module Main (main) where

import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Data.Text (pack)
import Text.Megaparsec (errorBundlePretty, parse)
import Data.Scientific (fromFloatDigits)

import JSON (jsonValue, JValue(..))

parseTest :: String -> JValue -> Assertion
parseTest input expected =
    case parse jsonValue "" (pack input) of
        Left err -> assertFailure $ "Parse error:\n" ++ errorBundlePretty err
        Right result -> assertEqual ("Parsing: " ++ input) expected result

testNull :: Assertion
testNull = parseTest "null" JNull

testTrue :: Assertion
testTrue = parseTest "true" (JBool True)

testFalse :: Assertion
testFalse = parseTest "false" (JBool False)

testNumber :: Assertion
testNumber = parseTest "123.45" (JNumber (fromFloatDigits (123.45 :: Double)))

testString :: Assertion
testString = parseTest "\"hello\"" (JString "hello")

testArray :: Assertion
testArray = parseTest "[null,true,1]" (JArray [JNull, JBool True, JNumber 1])

testObject :: Assertion
testObject = parseTest "{\"a\":null,\"b\":false}" (JObject [("a", JNull), ("b", JBool False)])

tests :: [Test]
tests =
    [ testCase "Test null parse" testNull
    , testCase "Test true parse" testTrue
    , testCase "Test false parse" testFalse
    , testCase "Test number parse" testNumber
    , testCase "Test string parse" testString
    , testCase "Test array parse" testArray
    , testCase "Test object parse" testObject
    ]

main :: IO ()
main = defaultMain tests
