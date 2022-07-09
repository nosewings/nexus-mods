module NexusMods (
  User (..),
  validate,
  getTrackedMods,
  trackMod,
  untrackMod,
  runNexus,
) where

import Data.Aeson.TH
import Data.Data
import Data.Functor
import Data.SOP.NS
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import NexusMods.TH
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

type NexusModsAPI =
  "v1" :> "users" :> "validate.json" :> Header' '[Required] "apikey" String :> Get '[JSON] User
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

api :: Proxy NexusModsAPI
api = Proxy

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

-- | Create the API functions.
validate :<|> getTrackedMods :<|> trackMod' :<|> untrackMod' = client api

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
