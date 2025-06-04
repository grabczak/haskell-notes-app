module JSONSpec (tests) where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Data.Text (pack)
import Text.Megaparsec (errorBundlePretty, parse)
import Data.Scientific (fromFloatDigits)

import JSON (JValue(..), fromJSON)

assertParseSuccess :: String -> JValue -> Assertion
assertParseSuccess input expected =
  case parse fromJSON "" (pack input) of
    Left err -> assertFailure $ "Parse error:\n" ++ errorBundlePretty err
    Right result -> assertEqual ("Parsing: " ++ input) expected result

assertParseFailure :: String -> Assertion
assertParseFailure input =
  case parse fromJSON "" (pack input) of
    Left _  -> return ()
    Right result -> assertFailure $ "    Expected parse failure, but got: " ++ show result

testNull :: Assertion
testNull = assertParseSuccess "null" JNull

testTrue :: Assertion
testTrue = assertParseSuccess "true" (JBool True)

testFalse :: Assertion
testFalse = assertParseSuccess "false" (JBool False)

testNumber :: Assertion
testNumber = assertParseSuccess "-123.45" (JNumber (fromFloatDigits (-123.45 :: Double)))

testString :: Assertion
testString = assertParseSuccess "\"hello\"" (JString "hello")

testArray :: Assertion
testArray = assertParseSuccess "[null,true,1]" (JArray [JNull, JBool True, JNumber 1])

testObject :: Assertion
testObject = assertParseSuccess "{\"a\":null,\"b\":false}" (JObject [("a", JNull), ("b", JBool False)])

testNegativeNumber :: Assertion
testNegativeNumber = assertParseSuccess "-21" (JNumber (fromFloatDigits (-21 :: Double)))

testEmptyString :: Assertion
testEmptyString = assertParseSuccess "\"\"" (JString "")

testCorrectFormat :: Assertion
testCorrectFormat = assertParseSuccess
  "{\"JSON Test Pattern pass3\":{\"The outermost value\":\"must be an object or array.\",\"In this test\":\"It is an object.\"}}"
  (JObject
    [ ("JSON Test Pattern pass3",
      JObject
        [ ("The outermost value", JString "must be an object or array.")
        , ("In this test", JString "It is an object.")
        ]
    )
    ]
  )

testDeepNesting :: Assertion
testDeepNesting = assertParseSuccess "[[[[[\"Deep nesting\"]]]]]" (JArray [JArray [JArray [JArray [JArray [JString "Deep nesting"]]]]])

testGeneralCase :: Assertion
testGeneralCase = assertParseSuccess
  "[\"JSON Test Pattern pass1\",{\"object with 1 member\":[\"array with 1 element\"]},{},[],-42,true,false,null,{\"integer\":1234567890,\"real\":-9876.54321,\"e\":1.23456789e-13,\"E\":1.23456789e+34,\"\":2.3456789012e+76,\"zero\":0,\"one\":1,\"space\":\" \",\"quote\":\"\\\"\",\"backslash\":\"\\\\\",\"controls\":\"\\b\\f\\n\\r\\t\",\"slash\":\"/ & /\",\"alpha\":\"abcdefghijklmnopqrstuvwyz\",\"ALPHA\":\"ABCDEFGHIJKLMNOPQRSTUVWYZ\",\"digit\":\"0123456789\",\"0123456789\":\"digit\",\"special\":\"`1~!@#$%^&*()_+-={':[,]}|;.</>?\",\"hex\":\"ģ䕧覫췯屴屴\",\"true\":true,\"false\":false,\"null\":null,\"array\":[],\"object\":{},\"address\":\"50 St. James Street\",\"url\":\"http://www.JSON.org/\",\"comment\":\"// /* <!-- --\",\"# -- --> */\":\" \",\" s p a c e d \":[1,2,3,4,5,6,7],\"compact\":[1,2,3,4,5,6,7],\"jsontext\":\"{\\\"object with 1 member\\\":[\\\"array with 1 element\\\"]}\",\"quotes\":\"&#34; \\\" %22 0x22 034 &#x22;\",\"/\\\\\\\"쫾몾ꮘﳞ볚屴\\b\\f\\n\\r\\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?\":\"A key can be any string\"},0.5,98.6,99.44,1066,10,1,0.1,1,2,2,\"rosebud\"]"
  (JArray
    [ JString "JSON Test Pattern pass1"
    , JObject [("object with 1 member", JArray [JString "array with 1 element"])]
    , JObject []
    , JArray []
    , JNumber (-42)
    , JBool True
    , JBool False
    , JNull
    , JObject
      [ ("integer", JNumber 1234567890)
      , ("real", JNumber (fromFloatDigits (-9876.54321 :: Double)))
      , ("e", JNumber (fromFloatDigits (1.23456789e-13 :: Double)))
      , ("E", JNumber (fromFloatDigits (1.23456789e+34 :: Double)))
      , ("", JNumber (fromFloatDigits (2.3456789012e+76 :: Double)))
      , ("zero", JNumber 0)
      , ("one", JNumber 1)
      , ("space", JString " ")
      , ("quote", JString "\"")
      , ("backslash", JString "\\")
      , ("controls", JString "\b\f\n\r\t")
      , ("slash", JString "/ & /")
      , ("alpha", JString "abcdefghijklmnopqrstuvwyz")
      , ("ALPHA", JString "ABCDEFGHIJKLMNOPQRSTUVWYZ")
      , ("digit", JString "0123456789")
      , ("0123456789", JString "digit")
      , ("special", JString "`1~!@#$%^&*()_+-={':[,]}|;.</>?")
      , ("hex", JString "ģ䕧覫췯屴屴")
      , ("true", JBool True)
      , ("false", JBool False)
      , ("null", JNull)
      , ("array", JArray [])
      , ("object", JObject [])
      , ("address", JString "50 St. James Street")
      , ("url", JString "http://www.JSON.org/")
      , ("comment", JString "// /* <!-- --")
      , ("# -- --> */", JString " ")
      , (" s p a c e d ", JArray (map JNumber [1,2,3,4,5,6,7]))
      , ("compact", JArray (map JNumber [1,2,3,4,5,6,7]))
      , ("jsontext", JString "{\"object with 1 member\":[\"array with 1 element\"]}")
      , ("quotes", JString "&#34; \" %22 0x22 034 &#x22;")
      , ("/\\\"쫾몾ꮘﳞ볚屴\b\f\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?", JString "A key can be any string")
      ]
    , JNumber (fromFloatDigits (0.5 :: Double))
    , JNumber (fromFloatDigits (98.6 :: Double))
    , JNumber (fromFloatDigits (99.44 :: Double))
    , JNumber 1066
    , JNumber 10
    , JNumber 1
    , JNumber (fromFloatDigits (0.1 :: Double))
    , JNumber 1
    , JNumber 2
    , JNumber 2
    , JString "rosebud"
    ]
  )

passedParseTests :: [Test]
passedParseTests =
  [ testCase "Null" testNull
  , testCase "True" testTrue
  , testCase "False" testFalse
  , testCase "Number" testNumber
  , testCase "String" testString
  , testCase "Array" testArray
  , testCase "Object" testObject
  , testCase "Negative number" testNegativeNumber
  , testCase "Empty string" testEmptyString
  , testCase "Correct format" testCorrectFormat
  , testCase "Deep nesting" testDeepNesting
  , testCase "General case" testGeneralCase
  ]

testMisspelledNull :: Assertion
testMisspelledNull = assertParseFailure "nulls"

testExplicitString :: Assertion
testExplicitString = assertParseFailure "Explicit string"

testUnclosedArray :: Assertion
testUnclosedArray = assertParseFailure "[\"Unclosed array\""

testUnquotedKey :: Assertion
testUnquotedKey = assertParseFailure "{unquotedKey:true}"

testTrailingCommaInArray :: Assertion
testTrailingCommaInArray = assertParseFailure "[\"Trailing comma\",]"

testDoubleCommaInArray :: Assertion
testDoubleCommaInArray = assertParseFailure "[\"Double comma\",,]"

testMissingValueInArray :: Assertion
testMissingValueInArray = assertParseFailure "[,\"Missing value\"]"

testCommaAfterClosedArray :: Assertion
testCommaAfterClosedArray = assertParseFailure "[\"Comma after closing bracket\"],"

testExtraBracket :: Assertion
testExtraBracket = assertParseFailure "[\"Extra bracket\"]]"

testTrailingCommaInObject :: Assertion
testTrailingCommaInObject = assertParseFailure "{\"Trailing comma\":true,}"

testExtraValueAfterObject :: Assertion
testExtraValueAfterObject = assertParseFailure "{\"key\":\"value\"}\"Extra value after object\""

testIllegalExpression :: Assertion
testIllegalExpression = assertParseFailure "{\"Illegal expression\":1+2}"

testIllegalInvocation :: Assertion
testIllegalInvocation = assertParseFailure "{\"Illegal invocation\":alert()}"

testHexNumber :: Assertion
testHexNumber = assertParseFailure "{\"Hex number\":0x1A}"

testIllegalEscape :: Assertion
testIllegalEscape = assertParseFailure "{\"Illegal escape\":\"\\x\"}"

testEscapeOutsideString :: Assertion
testEscapeOutsideString = assertParseFailure "[\n]"

testIncorrectNesting :: Assertion
testIncorrectNesting = assertParseFailure "[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]"

testMissingColon :: Assertion
testMissingColon = assertParseFailure "{\"key\"\"Missing colon\"}"

testDoubleColon :: Assertion
testDoubleColon = assertParseFailure "{\"key\"::\"Double colon\"}"

testCommaInsteadOfColonInObject :: Assertion
testCommaInsteadOfColonInObject = assertParseFailure "{\"key\",\"Comma instead of colon\"}"

testColonInsteadOfCommaInArray :: Assertion
testColonInsteadOfCommaInArray = assertParseFailure "[\"Colon instead of comma\":false]"

testIncorrectValue :: Assertion
testIncorrectValue = assertParseFailure "[\"Incorrect value\",truth]"

testSingleQuote :: Assertion
testSingleQuote = assertParseFailure "['single quote']"

testIncorrectExponent :: Assertion
testIncorrectExponent = assertParseFailure "[\"Incorrect exponent\",0e]"

testIncorrectPlusInExponent :: Assertion
testIncorrectPlusInExponent = assertParseFailure "[\"Incorrect plus in exponent\",0e+]"

testIncorrectMinusInExponent :: Assertion
testIncorrectMinusInExponent = assertParseFailure "[\"Incorrect minus in exponent\",0e+-1]"

testCommaInsteadOfClosingBrace :: Assertion
testCommaInsteadOfClosingBrace = assertParseFailure "{\"key\":\"value\","

testMismatch :: Assertion
testMismatch = assertParseFailure "[\"Mismatch\"}"

failedParseTests :: [Test]
failedParseTests =
  [ testCase "Misspelled null" testMisspelledNull
  , testCase "Explicit string" testExplicitString
  , testCase "Unclosed array" testUnclosedArray
  , testCase "Unquoted key" testUnquotedKey
  , testCase "Trailing comma in array" testTrailingCommaInArray
  , testCase "Double comma in array" testDoubleCommaInArray
  , testCase "Missing value in array" testMissingValueInArray
  , testCase "Comma after closed array" testCommaAfterClosedArray
  , testCase "Extra bracket" testExtraBracket
  , testCase "Trailing comma in object" testTrailingCommaInObject
  , testCase "Extra value after object" testExtraValueAfterObject
  , testCase "Illegal expression" testIllegalExpression
  , testCase "Illegal invocation" testIllegalInvocation
  , testCase "Hex number" testHexNumber
  , testCase "Illegal escape" testIllegalEscape
  , testCase "Escape outside string" testEscapeOutsideString
  , testCase "Incorrect nesting" testIncorrectNesting
  , testCase "Missing colon" testMissingColon
  , testCase "Double colon" testDoubleColon
  , testCase "Comma instead of colon in object" testCommaInsteadOfColonInObject
  , testCase "Colon instead of comma in array" testColonInsteadOfCommaInArray
  , testCase "Incorrect value" testIncorrectValue
  , testCase "Single quote" testSingleQuote
  , testCase "Incorrect exponent" testIncorrectExponent
  , testCase "Incorrect plus in exponent" testIncorrectPlusInExponent
  , testCase "Incorrect minus in exponent" testIncorrectMinusInExponent
  , testCase "Comma instead of closing brace" testCommaInsteadOfClosingBrace
  , testCase "Mismatch" testMismatch
  ]

tests :: [Test]
tests = [testGroup "Parse Success" passedParseTests, testGroup "Parse Failure" failedParseTests]
