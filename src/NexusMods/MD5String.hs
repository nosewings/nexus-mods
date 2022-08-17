module NexusMods.MD5String (
  MD5String,
  fromString,
  toString,
) where

import Data.Char
import Servant.API

newtype MD5String = MD5String String

instance ToHttpApiData MD5String where
  toUrlPiece = toUrlPiece . toString

lengthIs :: Int -> [a] -> Bool
lengthIs 0 [] = True
lengthIs n (_ : xs) = lengthIs (n - 1) xs
lengthIs _ _ = False

fromString :: String -> Maybe MD5String
fromString s
  | lengthIs 32 s && all isHexDigit s = Just (MD5String s)
  | otherwise = Nothing

toString :: MD5String -> String
toString (MD5String s) = s
