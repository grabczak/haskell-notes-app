module Main (main) where

import Test.Framework (defaultMain, Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Data.Text (pack)
import Text.Megaparsec (errorBundlePretty, parse)
import Data.Scientific (fromFloatDigits)

import JSON (jsonValue, JValue(..))

assertParseSuccess :: String -> JValue -> Assertion
assertParseSuccess input expected =
    case parse jsonValue "" (pack input) of
        Left err -> assertFailure $ "Parse error:\n" ++ errorBundlePretty err
        Right result -> assertEqual ("Parsing: " ++ input) expected result

assertParseFailure :: String -> Assertion
assertParseFailure input =
    case parse jsonValue "" (pack input) of
        Left _  -> return ()
        Right v -> assertFailure $ "Expected parse failure, but got: " ++ show v

testNull :: Assertion
testNull = assertParseSuccess "null" JNull

testTrue :: Assertion
testTrue = assertParseSuccess "true" (JBool True)

testFalse :: Assertion
testFalse = assertParseSuccess "false" (JBool False)

testNumber :: Assertion
testNumber = assertParseSuccess "123.45" (JNumber (fromFloatDigits (123.45 :: Double)))

testString :: Assertion
testString = assertParseSuccess "\"hello\"" (JString "hello")

testArray :: Assertion
testArray = assertParseSuccess "[null,true,1]" (JArray [JNull, JBool True, JNumber 1])

testObject :: Assertion
testObject = assertParseSuccess "{\"a\":null,\"b\":false}" (JObject [("a", JNull), ("b", JBool False)])

passedParseTests :: [Test]
passedParseTests =
    [ testCase "Test null" testNull
    , testCase "Test true" testTrue
    , testCase "Test false" testFalse
    , testCase "Test number" testNumber
    , testCase "Test string" testString
    , testCase "Test array" testArray
    , testCase "Test object" testObject
    ]

testExplicitString :: Assertion
testExplicitString = assertParseFailure "A JSON payload should be an object or array, not a string."

testUnclosedArray :: Assertion
testUnclosedArray = assertParseFailure "[\"Unclosed array\""

testUnquotedKey :: Assertion
testUnquotedKey = assertParseFailure "{unquotedKey:\"Keys must be quoted\"}"

testExtraComma :: Assertion
testExtraComma = assertParseFailure "[\"Extra comma\",]"

testDoubleExtraComma :: Assertion
testDoubleExtraComma = assertParseFailure "[\"Double extra comma\",,]"

testMissingValueInArray :: Assertion
testMissingValueInArray = assertParseFailure "[, \"<- Missing value\"]"

testCommaAfterClosedArray :: Assertion
testCommaAfterClosedArray = assertParseFailure "[\"Comma after close\"],"

testExtraBracket :: Assertion
testExtraBracket = assertParseFailure "[\"Extra bracket\"]]"

testExtraCommaInObject :: Assertion
testExtraCommaInObject = assertParseFailure "{\"Extra comma\":true,}"

testExtraValueAfterObject :: Assertion
testExtraValueAfterObject = assertParseFailure "{\"key\":\"value\"} \"Extra value after object\""

testIllegalExpression :: Assertion
testIllegalExpression = assertParseFailure "{\"Illegal expression\": 1 + 2}"

testIllegalInvocation :: Assertion
testIllegalInvocation = assertParseFailure "{\"Illegal invocation\": alert()}"

testLeadingZeroes :: Assertion
testLeadingZeroes = assertParseFailure "{\"Numbers cannot have leading zeroes\": 013}"

testHexNumber :: Assertion
testHexNumber = assertParseFailure "{\"Hex numbers are not allowed\": 0x1A}"

testIllegalBackslash :: Assertion
testIllegalBackslash = assertParseFailure "{\"Illegal backslash\": \"\\x\"}"

testEscapeOutsideString :: Assertion
testEscapeOutsideString = assertParseFailure "[\n]"

testIncorrectNesting :: Assertion
testIncorrectNesting = assertParseFailure "[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]"

testMissingColon :: Assertion
testMissingColon = assertParseFailure "{\"key\" \"missing colon\"}"

testDoubleColon :: Assertion
testDoubleColon = assertParseFailure "{\"key\":: \"double colon\"}"

testCommaInsteadOfColon :: Assertion
testCommaInsteadOfColon = assertParseFailure "{\"key\", \"comma instead of colon\"}"

testColonInsteadOfCommaInArray :: Assertion
testColonInsteadOfCommaInArray = assertParseFailure "[\"Colon instead of comma\": false]"

testIncorrectValue :: Assertion
testIncorrectValue = assertParseFailure "[\"Bad value\", truth]"

testSingleQuote :: Assertion
testSingleQuote = assertParseFailure "['single quote']"

testTabCharacterInString :: Assertion
testTabCharacterInString = assertParseFailure "\"Tab\tcharacter\""

testEscapedTabCharactersInString :: Assertion
testEscapedTabCharactersInString = assertParseFailure "\"Escaped tab\\tcharacter\""

testLineBreak :: Assertion
testLineBreak = assertParseFailure "[\"Line\nbreak\"]"

testIncorrectExponent :: Assertion
testIncorrectExponent = assertParseFailure "[\"Incorrect exponent\", 0e]"

testIncorrectPlusInExponent :: Assertion
testIncorrectPlusInExponent = assertParseFailure "[\"Incorrect plus in exponent\", 0e+]"

testIncorrectMinusInExponent :: Assertion
testIncorrectMinusInExponent = assertParseFailure "[\"Incorrect minus in exponent\", 0e+-1]"

testCommaInsteadOfClosingBrace :: Assertion
testCommaInsteadOfClosingBrace = assertParseFailure "{\"key\": \"value\","

testMismatch :: Assertion
testMismatch = assertParseFailure "[\"Mismatch\"}"

failedParseTests :: [Test]
failedParseTests =
    [ testCase "Test explicit string" testExplicitString
    , testCase "Test unclosed array" testUnclosedArray
    , testCase "Test unquoted key" testUnquotedKey
    , testCase "Test extra comma" testExtraComma
    , testCase "Test double extra comma" testDoubleExtraComma
    , testCase "Test missing value in array" testMissingValueInArray
    , testCase "Test comma after closed array" testCommaAfterClosedArray
    , testCase "Test extra bracket" testExtraBracket
    , testCase "Test extra comma in object" testExtraCommaInObject
    , testCase "Test extra value after object" testExtraValueAfterObject
    , testCase "Test illegal expression" testIllegalExpression
    , testCase "Test illegal invocation" testIllegalInvocation
    , testCase "Test leading zeroes" testLeadingZeroes
    , testCase "Test hex number" testHexNumber
    , testCase "Test illegal backslash" testIllegalBackslash
    , testCase "Test escape outside string" testEscapeOutsideString
    , testCase "Test too deep nesting" testIncorrectNesting
    , testCase "Test missing colon" testMissingColon
    , testCase "Test double colon" testDoubleColon
    , testCase "Test comma instead of colon" testCommaInsteadOfColon
    , testCase "Test colon instead of comma in array" testColonInsteadOfCommaInArray
    , testCase "Test incorrect value" testIncorrectValue
    , testCase "Test single quote" testSingleQuote
    , testCase "Test tab character in string" testTabCharacterInString
    , testCase "Test escaped tab characters in string" testEscapedTabCharactersInString
    , testCase "Test line break" testLineBreak
    , testCase "Test incorrect exponent" testIncorrectExponent
    , testCase "Test incorrect plus in exponent" testIncorrectPlusInExponent
    , testCase "Test incorrect minus in exponent" testIncorrectMinusInExponent
    , testCase "Test comma instead of closing brace" testCommaInsteadOfClosingBrace
    , testCase "Test mismatch" testMismatch
    ]

tests :: [Test]
tests = [testGroup "Successful Parse" passedParseTests, testGroup "Failed Parse" failedParseTests]

main :: IO ()
main = defaultMain tests
