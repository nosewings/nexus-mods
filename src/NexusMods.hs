module NexusMods (
  User (..),
  validate,
  getTrackedMods,
  runNexus,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Data
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client

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

deriveFromJSON (defaultOptions {fieldLabelModifier = camelTo2 '_'}) ''User

data ModRef = ModRef
  { modId :: Int,
    domainName :: String
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON (defaultOptions {fieldLabelModifier = camelTo2 '_'}) ''ModRef

type NexusModsAPI =
  "v1" :> "users" :> "validate.json" :> Header' '[Required] "apikey" String :> Get '[JSON] User
    :<|> "v1" :> "user" :> "tracked_mods.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [ModRef]

api :: Proxy NexusModsAPI
api = Proxy

-- | Validate a user's API key and return their info.
validate :: String -> ClientM User

-- | Get a user's list of tracked mods.
getTrackedMods :: String -> ClientM [ModRef]
validate :<|> getTrackedMods = client api

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
