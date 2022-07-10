module NexusMods (
  Changelogs,
  User (..),
  ModRef (..),
  Colour (..),
  ColourScheme (..),
  getChangelogs,
  validate,
  getTrackedMods,
  trackMod,
  untrackMod,
  getColourSchemes,
  runNexus,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Data
import Data.Functor
import Data.Map
import Data.SOP.NS
import Data.Text qualified as Text
import Data.Word
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import NexusMods.TH
import Servant.API
import Servant.Client
import Text.ParserCombinators.ReadP

impossible :: a
impossible = error "an impossible situation has occurred"

-- | A list of mod changelogs.
type Changelogs = Map String [String]

-- | Details about a Nexus Mods user.
data User = User
  { userId :: Int,
    key :: String,
    email :: String,
    profileUrl :: String,
    isPremium :: Bool,
    isSupporter :: Bool
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''User

data ModRef = ModRef
  { modId :: Int,
    domainName :: String
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''ModRef

newtype Message = Message
  { message :: String
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''Message

data Colour = Colour
  { red :: Word8,
    blue :: Word8,
    green :: Word8
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON Colour where
  parseJSON = withText "Colour" \t ->
    case readP_to_S colour (Text.unpack t) of
      [] -> fail ("expected an RGB color string; got " ++ Text.unpack t)
      [(colour, "")] -> return colour
      _ -> impossible
   where
    colour = char '#' *> (Colour <$> hexWord8 <*> hexWord8 <*> hexWord8) <* eof
    hexWord8 = combine <$> hexDigit <*> hexDigit
     where
      combine hi lo = 16 * hi + lo
    hexDigit =
      satisfy isHexDigit <&> \c ->
        fromIntegral
          if
              | isDigit c -> ord c - ord '0'
              | isAsciiUpper c -> ord c - ord 'A'
              | isAsciiLower c -> ord c - ord 'a'
              | otherwise -> impossible

data ColourScheme = ColourScheme
  { id :: Int,
    name :: String,
    primaryColour :: Colour,
    secondaryColour :: Colour,
    darkerColour :: Colour
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''ColourScheme

type NexusModsAPI =
  "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> Capture "mod_id" Int :> "changelogs.json" :> Header' '[Required] "apikey" String :> Get '[JSON] Changelogs
    :<|> "v1" :> "users" :> "validate.json" :> Header' '[Required] "apikey" String :> Get '[JSON] User
    :<|> "v1" :> "user" :> "tracked_mods.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [ModRef]
    :<|> ( "v1" :> "user" :> "tracked_mods.json"
            :> Header' '[Required] "apikey" String
            :> QueryParam' '[Required] "domain_name" String
            :> QueryParam' '[Required] "mod_id" Int
            :> UVerb POST '[JSON] '[WithStatus 200 Message, WithStatus 201 Message]
         )
    :<|> ( "v1" :> "user" :> "tracked_mods.json"
            :> Header' '[Required] "apikey" String
            :> QueryParam' '[Required] "domain_name" String
            :> QueryParam' '[Required] "mod_id" Int
            :> UVerb DELETE '[JSON] [WithStatus 200 Message, WithStatus 404 Message]
         )
    :<|> "v1" :> "colourschemes" :> Header' '[Required] "apikey" String :> Get '[JSON] [ColourScheme]

api :: Proxy NexusModsAPI
api = Proxy

-- | Get a mod's list of changelogs.
getChangelogs :: String -> Int -> String -> ClientM Changelogs

-- | Validate a user's API key and return their info.
validate :: String -> ClientM User

-- | Get a user's list of tracked mods.
getTrackedMods :: String -> ClientM [ModRef]

-- | Internal version of @trackMod@.
trackMod' :: String -> String -> Int -> ClientM (Union '[WithStatus 200 Message, WithStatus 201 Message])

-- | Start tracking a mod.  Returns @True@ if the user was not already
-- tracking the mod.
trackMod :: String -> String -> Int -> ClientM Bool
trackMod k d m =
  trackMod' k d m <&> \case
    Z _ -> False
    _ -> True

-- | Internal version of @untrackMod@.
untrackMod' :: [Char] -> [Char] -> Int -> ClientM (Union '[WithStatus 200 Message, WithStatus 404 Message])

-- | Stop tracking a mod.  Returns @True@ if the user was previously
-- tracking the mod.
untrackMod :: String -> String -> Int -> ClientM Bool
untrackMod a b c =
  untrackMod' a b c <&> \case
    Z _ -> True
    _ -> False

-- | Get a list of all colour schemes.
getColourSchemes :: String -> ClientM [ColourScheme]

-- | Create the API functions.
getChangelogs :<|> validate :<|> getTrackedMods :<|> trackMod' :<|> untrackMod' :<|> getColourSchemes = client api

-- | Run a Nexus Mods API computation.  This is a convenience function
-- that uses HTTPS and the default Nexus Mods URL.
runNexus :: ClientM a -> IO (Either ClientError a)
runNexus m = do
  manager <- newManager tlsManagerSettings
  runClientM m . mkClientEnv manager $
    BaseUrl
      { baseUrlScheme = Https,
        baseUrlHost = "api.nexusmods.com",
        baseUrlPort = 443,
        baseUrlPath = ""
      }
