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
import Data.Char (toCharCode, toLower, fromCharCode)
import Data.String (length, toCharArray, fromCharArray)
import Data.String.Unsafe (charAt)
import Data.Tuple (uncurry)

type SecretKey = String
type PlainText = String
type CipherText = String

charToInt :: Char -> Int
charToInt = toLower >>> toCharCode

a :: Int
a = toCharCode 'a'

z :: Int
z = toCharCode 'z'

substitute :: Char -> Char -> Char
substitute keyChar msgChar =
  let keyInt = charToInt keyChar in
  let msgInt = charToInt msgChar in
  fromCharCode $ (+) a $ mod (keyInt + msgInt) a

encode :: SecretKey -> PlainText -> CipherText
encode key msg = fromCharArray $ (uncurry encodeChar) <$> zip (toCharArray msg) (range 0 $ length msg - 1)
  where
  encodeChar c i =
    let keyChar = charAt (mod i $ length key) key in
    substitute keyChar c

decode :: SecretKey -> CipherText -> PlainText
decode key ct = "decodeme"

decipher :: CipherText -> PlainText -> SecretKey
decipher ct msg = "decipherme"
