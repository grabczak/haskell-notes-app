module Main (main) where

import Test.Framework (defaultMain)
import JSONSpec (tests)

main :: IO ()
main = defaultMain tests
