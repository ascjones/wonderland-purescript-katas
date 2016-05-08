module AlphabetCipher
  ( SecretKey
  , PlainText
  , CipherText
  , encode
  , decode
  , decipher
  ) where

import Prelude

type SecretKey = String
type PlainText = String
type CipherText = String

encode :: SecretKey -> PlainText -> CipherText
encode key msg = "encodeme"

decode :: SecretKey -> CipherText -> PlainText
decode key ct = "decodeme"

decipher :: CipherText -> PlainText -> SecretKey
decipher ct msg = "decipherme"
