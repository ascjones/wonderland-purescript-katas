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

mapChars :: (Int -> Int -> Char) -> SecretKey -> String -> String
mapChars f key msg = fromCharArray $ (uncurry f) <$> mapWithKeyChars
  where
  mapWithKeyChars =
    let keyChars = (\i -> charToInt $ charAt (mod i $ length key) key) <$> (range 0 $ length msg -1) in
    zip (charToInt <$> toCharArray msg) keyChars

encode :: SecretKey -> PlainText -> CipherText
encode = mapChars encodeChar
  where
  encodeChar :: Int -> Int -> Char
  encodeChar keyInt msgInt =
    fromCharCode $ (+) a $ mod (keyInt + msgInt) 26

decode :: SecretKey -> CipherText -> PlainText
decode = mapChars decodeChar
  where
  decodeChar keyInt msgInt =
    let diff = keyInt - msgInt in
    fromCharCode $ (+) a $ mod (diff + 26) 26

decipher :: CipherText -> PlainText -> SecretKey
decipher ct msg = "decipherme"
