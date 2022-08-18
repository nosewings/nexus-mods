module NexusMods.MD5String (
  MD5String,
  fromString,
  toString,
) where

import Control.Applicative
import Control.Category ((>>>))
import Data.Char
import Servant.API
import Text.Read

newtype MD5String = MD5String String
  deriving (Eq, Ord, Show)

-- No `Generic` instance

instance Read MD5String where
  readPrec = go >>= (fromString >>> maybe empty return)
   where
    go = parens . prec 10 $ do
      Ident "MD5String" <- lexP
      readPrec

instance ToHttpApiData MD5String where
  toUrlPiece = toUrlPiece . toString

lengthIs :: Int -> [a] -> Bool
lengthIs 0 [] = True
lengthIs n (_ : xs) | n > 0 = lengthIs (n - 1) xs
lengthIs _ _ = False

fromString :: String -> Maybe MD5String
fromString s
  | lengthIs 32 s && all isHexDigit s = Just (MD5String s)
  | otherwise = Nothing

toString :: MD5String -> String
toString (MD5String s) = s
