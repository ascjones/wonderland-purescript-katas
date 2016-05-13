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

a :: Int
a = toCharCode 'a'

z :: Int
z = toCharCode 'z'

charToInt :: Char -> Int
charToInt = toLower >>> toCharCode >>> toZeroBased
  where toZeroBased ch = ch - a

abs :: Int -> Int
abs i = if i > 0 then i else i * -1

--   0 1 2 3
-- 0 0 1 2 3
-- 1 1 2 3 0
-- 2 2 3 0 1
-- 3 3 0 1 2

-- 0 1 2 3 0 1 2 3
-- 3 - 2 = 1
-- 2 - 3 = -1

substitute :: Char -> Char -> Char
substitute keyChar msgChar =
  let keyInt = charToInt keyChar in
  let msgInt = charToInt msgChar in
  fromCharCode $ (+) a $ mod (keyInt + msgInt) 26

encode :: SecretKey -> PlainText -> CipherText
encode key msg = fromCharArray $ (uncurry encodeChar) <$> zip (toCharArray msg) (range 0 $ length msg - 1)
  where
  encodeChar c i =
    let keyChar = charAt (mod i $ length key) key in
    substitute keyChar c

decode :: SecretKey -> CipherText -> PlainText
decode key ct = fromCharArray $ (uncurry decodeChar) <$> zip (toCharArray ct) (range 0 $ length ct - 1)
  where
  decodeChar c i =
    let keyChar = charAt (mod i $ length key) key in
    let keyInt = charToInt keyChar in
    let msgInt = charToInt c in
    let diff = msgInt - keyInt in
    fromCharCode $ (+) a $ if diff >= 0 then diff else diff + 26 -- todo make this function better

decipher :: CipherText -> PlainText -> SecretKey
decipher ct msg = "decipherme"
