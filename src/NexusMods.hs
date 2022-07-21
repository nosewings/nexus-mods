module NexusMods (
  Period (..),
  ModUpdate (..),
  Changelogs,
  PublishedModInfo (..),
  ModStatus (..),
  ModUser (..),
  ModEndorsement (..),
  Mod (..),
  FileCategory (..),
  FileUpdate (..),
  FileDetails (..),
  ModFiles (..),
  MD5Lookup (..),
  DownloadLink (..),
  Category (..),
  Game (..),
  User (..),
  ModRef (..),
  EndorsementStatus (..),
  Endorsement (..),
  Colour (..),
  ColourScheme (..),
  getUpdates,
  getChangelogs,
  getLatestAdded,
  getLatestUpdated,
  getTrending,
  getMod,
  getModByHash,
  endorse,
  abstain,
  getModFiles,
  getFile,
  getDownloadLink,
  getGames,
  getGame,
  validate,
  getTrackedMods,
  trackMod,
  untrackMod,
  getEndorsements,
  getColourSchemes,
  runNexus,
) where

import Data.Functor
import Data.SOP.NS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import NexusMods.Internal
import Servant.API
import Servant.Client

-- | Get a list of mod updates within a given time period.
getUpdates :: String -> Period -> String -> ClientM [ModUpdate]

-- | Get a mod's list of changelogs.
getChangelogs :: String -> Int -> String -> ClientM Changelogs

-- | Get the ten most recently added mods for a given game.
getLatestAdded :: String -> String -> ClientM [Mod]

-- | Get the ten most recently updated mods for a given game.
getLatestUpdated :: String -> String -> ClientM [Mod]

-- | Get ten trending mods for a given game.
getTrending :: String -> String -> ClientM [Mod]

-- | Internal version of @getMod@.
getMod' :: String -> String -> String -> ClientM Mod

-- | Get a mod by game and ID.
getMod :: String -> Int -> String -> ClientM Mod
getMod gameDomainName id = getMod' gameDomainName (show id ++ ".json")

-- | Internal version of getModByHash.
getModByHash' :: String -> String -> String -> ClientM [MD5Lookup]

-- | Given an MD5 hash, get all mods that have a file with that hash.
getModByHash :: String -> String -> String -> ClientM [MD5Lookup]
getModByHash gameDomainName md5Hash = getModByHash' gameDomainName (md5Hash ++ ".json")

-- | Internal version of @endorse@.
endorse' :: String -> Int -> EndorseVersion -> String -> ClientM Message

-- | Endorse a mod.
endorse :: String -> Int -> Maybe String -> String -> ClientM Message
endorse gameDomainName id version = endorse' gameDomainName id (EndorseVersion version)

-- | Internal version of @abstain@..
abstain' :: String -> Int -> EndorseVersion -> String -> ClientM Message

-- | Stop endorsing a mod.
abstain :: String -> Int -> Maybe String -> String -> ClientM Message
abstain gameDomainName id version = abstain' gameDomainName id (EndorseVersion version)

-- | Internal version of @getModFiles@.
getModFiles' :: String -> Int -> Maybe [FileCategory] -> String -> ClientM ModFiles

-- | Get a mod's list of files.
getModFiles :: String -> Int -> [FileCategory] -> String -> ClientM ModFiles
getModFiles gameDomainName md5Hash [] = getModFiles' gameDomainName md5Hash Nothing
getModFiles gameDomainName md5Hash fileCategories = getModFiles' gameDomainName md5Hash (Just fileCategories)

-- | Internal version of @getFile@.
getFile' :: String -> Int -> String -> String -> ClientM FileDetails

-- | Get a specific file.
getFile :: String -> Int -> Int -> String -> ClientM FileDetails
getFile gameDomainName modId fileId = getFile' gameDomainName modId (show fileId ++ ".json")

-- | Internal version of @getDownloadLink@.
getDownloadLink' :: String -> Int -> Int -> Maybe String -> Maybe DownloadExpiry -> String -> ClientM [DownloadLink]

-- | Get a download link for a mod file.
getDownloadLink :: String -> Int -> Int -> Maybe (String, POSIXTime) -> String -> ClientM [DownloadLink]
getDownloadLink gameDomainName modId id keyExpires = getDownloadLink' gameDomainName modId id key expires
 where
  key = fst <$> keyExpires
  expires = DownloadExpiry . snd <$> keyExpires

-- | Get all games.
getGames :: Maybe Bool -> String -> ClientM [Game]

-- | Internal version of @getGame@.  This is necessary because the API
-- path is @/v1/games/{game_domain_name}.json@, but Servant does not
-- (as far as I know) give us the ability to modify a captured path
-- component, so we have to add the @.json@ on ourselves.
getGame' :: String -> String -> ClientM Game

-- | Get a specific game.
getGame :: String -> String -> ClientM Game
getGame gameDomainName = getGame' (gameDomainName ++ ".json")

-- | Validate a user's API key and return their info.
validate :: String -> ClientM User

-- | Get a user's list of tracked mods.
getTrackedMods :: String -> ClientM [ModRef]

-- | Internal version of @trackMod@.
trackMod' :: String -> Int -> String -> ClientM (Union '[WithStatus 200 Message, WithStatus 201 Message])

-- | Start tracking a mod.  Returns @True@ if the user was not already
-- tracking the mod.
trackMod :: String -> Int -> String -> ClientM Bool
trackMod domainName modId apikey =
  trackMod' domainName modId apikey <&> \case
    Z _ -> False
    _ -> True

-- | Internal version of @untrackMod@.
untrackMod' :: String -> Int -> String -> ClientM (Union '[WithStatus 200 Message, WithStatus 404 Message])

-- | Stop tracking a mod.  Returns @True@ if the user was previously
-- tracking the mod.
untrackMod :: String -> Int -> String -> ClientM Bool
untrackMod domainName modId apikey =
  untrackMod' domainName modId apikey <&> \case
    Z _ -> True
    _ -> False

-- | Get a list of the user's endorsements.
getEndorsements :: String -> ClientM [Endorsement]

-- | Get a list of all colour schemes.
getColourSchemes :: String -> ClientM [ColourScheme]

-- | Create the API functions.
getUpdates
  :<|> getChangelogs
  :<|> getLatestAdded
  :<|> getLatestUpdated
  :<|> getTrending
  :<|> getMod'
  :<|> getModByHash'
  :<|> endorse'
  :<|> abstain'
  :<|> getModFiles'
  :<|> getFile'
  :<|> getDownloadLink'
  :<|> getGames
  :<|> getGame'
  :<|> validate
  :<|> getTrackedMods
  :<|> trackMod'
  :<|> untrackMod'
  :<|> getEndorsements
  :<|> getColourSchemes =
    client api

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
