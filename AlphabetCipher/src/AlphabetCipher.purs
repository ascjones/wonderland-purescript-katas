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

--   0 1 2 3
-- 0 0 1 2 3
-- 1 1 2 3 0
-- 2 2 3 0 1
-- 3 3 0 1 2

substitute :: Char -> Char -> Char
substitute keyChar msgChar =
  let keyInt = charToInt keyChar in
  let msgInt = charToInt msgChar in
  fromCharCode $ (+) a $ mod (keyInt + msgInt) 26

mapChars :: (Char -> Char -> Char) -> SecretKey -> String -> String
mapChars f key msg = fromCharArray $ (uncurry f) <$> mapWithKeyChars
  where
  mapWithKeyChars =
    let keyChars = (\i -> charAt (mod i $ length key) key) <$> (range 0 $ length msg -1) in
    zip (toCharArray msg) keyChars

encode :: SecretKey -> PlainText -> CipherText
encode = mapChars substitute

decode :: SecretKey -> CipherText -> PlainText
decode key ct = fromCharArray $ (uncurry decodeChar) <$> zip (toCharArray ct) (range 0 $ length ct - 1)
  where
  decodeChar c i =
    let keyChar = charAt (mod i $ length key) key in
    let keyInt = charToInt keyChar in
    let msgInt = charToInt c in
    let diff = msgInt - keyInt in
    fromCharCode $ (+) a $ mod (diff + 26) 26 -- todo make this function better

decipher :: CipherText -> PlainText -> SecretKey
decipher ct msg = "decipherme"
