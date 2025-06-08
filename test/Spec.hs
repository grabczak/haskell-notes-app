module Main (main) where

import Test.Framework (defaultMain)

import JSONCodecSpec (tests)

main :: IO ()
main = defaultMain tests
