module NexusMods (
  Category (..),
  Game (..),
  Period (..),
  UTCTime,
  ModUpdate (..),
  PublishedModInfo (..),
  ModStatus (..),
  ModUser (..),
  EndorsementStatus (..),
  ModEndorsement (..),
  Mod (..),
  FileCategory (..),
  FileDetails (..),
  MD5Lookup (..),
  Changelogs,
  FileUpdate (..),
  ModFiles (..),
  POSIXTime,
  DownloadLink (..),
  User (..),
  Endorsement (..),
  ModRef (..),
  Colour (..),
  ColourScheme (..),
  getGames,
  getGame,
  getUpdates,
  getLatestAdded,
  getLatestUpdated,
  getTrending,
  getModByHash,
  getMod,
  getChangelogs,
  getModFiles,
  getFile,
  getDownloadLink,
  endorse,
  abstain,
  validate,
  getEndorsements,
  getTrackedMods,
  trackMod,
  untrackMod,
  getColourSchemes,
  runNexus,
) where

import Data.Functor
import Data.List.NonEmpty
import Data.SOP.NS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import NexusMods.Internal
import Servant.API
import Servant.API.Flatten
import Servant.Client

-- | Get all games.
getGames :: String -> Maybe Bool -> ClientM [Game]

-- | Internal version of @getGame@.
getGame' :: String -> JSONExt String -> ClientM Game

-- | Get a specific game.
getGame :: String -> String -> ClientM Game
getGame apikey gameDomainName = getGame' apikey (JSONExt gameDomainName)

-- | Get a list of mod updates within a given time period.
getUpdates :: String -> String -> Period -> ClientM [ModUpdate]

-- | Get the ten most recently added mods for a given game.
getLatestAdded :: String -> String -> ClientM [Mod]

-- | Get the ten most recently updated mods for a given game.
getLatestUpdated :: String -> String -> ClientM [Mod]

-- | Get ten trending mods for a given game.
getTrending :: String -> String -> ClientM [Mod]

-- | Internal version of getModByHash.
getModByHash' :: String -> String -> JSONExt MD5String -> ClientM [MD5Lookup]

-- | Given an MD5 hash, get all mods that have a file with that hash.
getModByHash :: String -> String -> MD5String -> ClientM [MD5Lookup]
getModByHash apikey gameDomainName md5Hash = getModByHash' apikey gameDomainName (JSONExt md5Hash)

-- | Internal version of @getMod@.
getMod' :: String -> String -> JSONExt Int -> ClientM Mod

-- | Get a mod by game and ID.
getMod :: String -> String -> Int -> ClientM Mod
getMod apikey gameDomainName id = getMod' apikey gameDomainName (JSONExt id)

-- | Get a mod's list of changelogs.
getChangelogs :: String -> String -> Int -> ClientM Changelogs

-- | Internal version of @getModFiles@.
getModFiles' :: String -> String -> Int -> Maybe FileCategories -> ClientM ModFiles

-- | Get a mod's list of files.
getModFiles :: String -> String -> Int -> [FileCategory] -> ClientM ModFiles
getModFiles apikey gameDomainName md5Hash [] = getModFiles' apikey gameDomainName md5Hash Nothing
getModFiles apikey gameDomainName md5Hash (c : cs) = getModFiles' apikey gameDomainName md5Hash (Just (FileCategories (c :| cs)))

-- | Internal version of @getFile@.
getFile' :: String -> String -> Int -> JSONExt Int -> ClientM FileDetails

-- | Get a specific file.
getFile :: String -> String -> Int -> Int -> ClientM FileDetails
getFile apikey gameDomainName modId fileId = getFile' apikey gameDomainName modId (JSONExt fileId)

-- | Internal version of @getDownloadLink@.
getDownloadLink' :: String -> String -> Int -> Int -> Maybe String -> Maybe DownloadExpiry -> ClientM [DownloadLink]

-- | Get a download link for a mod file.
getDownloadLink :: String -> String -> Int -> Int -> Maybe (String, POSIXTime) -> ClientM [DownloadLink]
getDownloadLink apikey gameDomainName modId id keyExpires = getDownloadLink' apikey gameDomainName modId id key expires
 where
  key = fst <$> keyExpires
  expires = DownloadExpiry . snd <$> keyExpires

-- | Internal version of @endorse@.
endorse' :: String -> String -> Int -> EndorseVersion -> ClientM MessageWithStatus

-- | Endorse a mod.
endorse :: String -> String -> Int -> Maybe String -> ClientM MessageWithStatus
endorse apikey gameDomainName id version = endorse' apikey gameDomainName id (EndorseVersion version)

-- | Internal version of @abstain@..
abstain' :: String -> String -> Int -> EndorseVersion -> ClientM MessageWithStatus

-- | Stop endorsing a mod.
abstain :: String -> String -> Int -> Maybe String -> ClientM MessageWithStatus
abstain apikey gameDomainName id version = abstain' apikey gameDomainName id (EndorseVersion version)

-- | Validate a user's API key and return their info.
validate :: String -> ClientM User

-- | Get a list of the user's endorsements.
getEndorsements :: String -> ClientM [Endorsement]

-- | Get a user's list of tracked mods.
getTrackedMods :: String -> ClientM [ModRef]

-- | Internal version of @trackMod@.
trackMod' :: String -> String -> Int -> ClientM (Union '[WithStatus 200 MessageWithStatus, WithStatus 201 MessageWithStatus])

-- | Start tracking a mod.  Returns @True@ if the user was not already
-- tracking the mod.
trackMod :: String -> String -> Int -> ClientM Bool
trackMod apikey domainName modId =
  trackMod' apikey domainName modId <&> \case
    Z _ -> False
    _ -> True

-- | Internal version of @untrackMod@.
untrackMod' :: String -> String -> Int -> ClientM (Union '[WithStatus 200 MessageWithStatus, WithStatus 404 MessageWithStatus])

-- | Stop tracking a mod.  Returns @True@ if the user was previously
-- tracking the mod.
untrackMod :: String -> String -> Int -> ClientM Bool
untrackMod apikey domainName modId =
  untrackMod' apikey domainName modId <&> \case
    Z _ -> True
    _ -> False

-- | Get a list of all colour schemes.
getColourSchemes :: String -> ClientM [ColourScheme]

-- | Create the API functions.
getGames
  :<|> getGame'
  :<|> getUpdates
  :<|> getLatestAdded
  :<|> getLatestUpdated
  :<|> getTrending
  :<|> getMod'
  :<|> getModByHash'
  :<|> endorse'
  :<|> abstain'
  :<|> getChangelogs
  :<|> getModFiles'
  :<|> getFile'
  :<|> getDownloadLink'
  :<|> validate
  :<|> getTrackedMods
  :<|> trackMod'
  :<|> untrackMod'
  :<|> getEndorsements
  :<|> getColourSchemes =
    client (flatten api)

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
