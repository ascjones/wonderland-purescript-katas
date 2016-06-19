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
import Data.Array.Unsafe (index)
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

strToIntChars :: String -> Array Int
strToIntChars text = charToInt <$> toCharArray text

repeatKey :: Array Char -> Int -> Array Char
repeatKey key fillCount = (\i -> charToInt $ charAt (mod i $ keyLength) key) <$> range 0 (fillCount - 1)
  where keyLength = length key

--   0 1 2 3
-- 0 0 1 2 3
-- 1 1 2 3 0
-- 2 2 3 0 1
-- 3 3 0 1 2

mapChars :: (Int -> Int -> Char) -> SecretKey -> String -> String
mapChars f key msg = fromCharArray $ (uncurry f) <$> mapWithKeyChars
  where
  mapWithKeyChars =
    -- let keyChars = (\i -> charToInt $ charAt (mod i $ length key) key) <$> (range 0 $ length msg -1) in
    zip (strToIntChars msg) (repeatKey (toCharArray key) (length msg))

encode :: SecretKey -> PlainText -> CipherText
encode = mapChars encodeChar
  where
  encodeChar :: Int -> Int -> Char
  encodeChar key msg =
    fromCharCode $ (+) a $ mod (key + msg) 26

decode :: SecretKey -> CipherText -> PlainText
decode = mapChars decodeChar
  where
  decodeChar :: Int -> Int -> Char
  decodeChar key msg =
    fromCharCode $ (+) a $ mod (key - msg + 26) 26

-- Plain text ROW: Secret Key
decipher :: CipherText -> PlainText -> SecretKey
decipher ct msg = extractKey $ toCharArray keys
  where

  keys = fromCharArray $ (uncurry decipherChar) <$> zip (strToIntChars ct) (strToIntChars msg)

  decipherChar :: Int -> Int -> Char
  decipherChar ciph plain =
    fromCharCode $ (+) a $ mod (ciph - plain + 26) 26

  extractKey ::Array Char -> Array Char
  extractKey = extractKey' []
    where
    extractKey' acc [] = acc
    extractKey' acc xs = -- todo: accumulate chars, test isRepeating, return

  isRepeating :: Array Char -> Bool
  isRepeating keyChars = keyString == repeatedKeys
    where
    repeatedKeys = repeatKeys keyChars (length keyString)

  -- extractKey :: Array Char -> Array Char
  -- extractKey keyString = extractKey' [] $ range 0 lastIndex
  --   where
  --   extractKey' acc lastIndex = acc { key = index keyString lastIndex }
  --   extractKey' acc xs =
  --     let currChar = head xs in
  --     if acc.end < 0
  --     then extractKey' { end : 0, lastChar : currChar, matching : false, key : [] }
  --     else if currChar == acc.lastChar && acc.end <
  --   keyStrLength = length keyString
  --   lastIndex = keyStrLength - 1
