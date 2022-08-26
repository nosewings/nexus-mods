module NexusMods.MD5String (
  MD5String,
  fromString,
  fromText,
  toString,
) where

import Data.Text (Text)
import Data.Text qualified as Text
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

instance FromHttpApiData MD5String where
  -- NOTE error message taken from the NexusMods server
  parseUrlPiece = maybe (Left "That is not a valid MD5 Hash") Right . fromText

lengthIs :: Int -> [a] -> Bool
lengthIs 0 [] = True
lengthIs n (_ : xs) | n > 0 = lengthIs (n - 1) xs
lengthIs _ _ = False

fromString :: String -> Maybe MD5String
fromString s
  | lengthIs 32 s && all isHexDigit s = Just (MD5String s)
  | otherwise = Nothing

-- Different from `fromString` because `Text.length s == 32` is much
-- faster than `lengthIs 32 s`.
fromText :: Text -> Maybe MD5String
fromText s
  | Text.length s == 32 && Text.all isHexDigit s = Just . MD5String . Text.unpack $ s
  | otherwise = Nothing

toString :: MD5String -> String
toString (MD5String s) = s
