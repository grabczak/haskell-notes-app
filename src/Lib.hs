{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib (
  hash,
  verify,
) where

import Data.Password.Argon2
import qualified Data.Text as T

hash :: String -> IO T.Text
hash _password = do
  let plain = mkPassword (T.pack _password)
  hashed <- hashPassword plain
  return $ T.pack (show hashed)

verify :: String -> T.Text -> IO Bool
verify _password _hash = do
  let plain = mkPassword (T.pack _password)
  let hashed = read (T.unpack _hash)
  return $ case checkPassword plain hashed of
    PasswordCheckSuccess -> True
    _ -> False
