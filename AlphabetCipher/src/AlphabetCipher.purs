module AlphabetCipher
  ( SecretKey
  , PlainText
  , CipherText
  , encode
  , decode
  , decipher
  ) where

import Prelude
import Data.Array (zip, range)
import Data.String (toCharArray, fromCharArray, length)
import Data.Tuple (uncurry)

type SecretKey = String
type PlainText = String
type CipherText = String

encode :: SecretKey -> PlainText -> CipherText
encode key msg = fromCharArray $ (uncurry encodeChar) <$> zip (toCharArray msg) (range 0 $ length msg - 1)
  where
  encodeChar c i = c

decode :: SecretKey -> CipherText -> PlainText
decode key ct = "decodeme"

decipher :: CipherText -> PlainText -> SecretKey
decipher ct msg = "decipherme"
