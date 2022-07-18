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

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.Data
import Data.Foldable
import Data.Function
import Data.Functor
import Data.HashMap.Strict qualified as HashMap
import Data.Map (Map)
import Data.Maybe
import Data.SOP.NS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import NexusMods.TH
import Servant.API
import Servant.Client
import Text.ParserCombinators.ReadP
import Web.FormUrlEncoded

impossible :: a
impossible = error "an impossible situation has occurred"

data Period = Day | Week | Month
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance ToHttpApiData Period where
  toQueryParam Day = "1d"
  toQueryParam Week = "1w"
  toQueryParam Month = "1m"

data ModUpdate = ModUpdate
  { modId :: Int,
    latestFileUpdate :: POSIXTime,
    latestModActivity :: POSIXTime
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''ModUpdate

-- | A list of mod changelogs.
type Changelogs = Map String [String]

data PublishedModInfo = PublishedModInfo
  { name :: String,
    summary :: String,
    description :: String,
    pictureUrl :: String,
    modDownloads :: Int,
    modUniqueDownloads :: Int
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''PublishedModInfo

data ModStatus
  = NotPublished
  | Published PublishedModInfo
  | Hidden
  deriving (Eq, Ord, Read, Show)

data ModUser = ModUser
  { memberId :: Int,
    memberGroupId :: Int,
    name :: String
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''ModUser

-- TODO Is there anything else?
data EndorsementStatus = Endorsed | Abstained
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance FromJSON EndorsementStatus where
  parseJSON = withText "EndorsementStatus" \case
    "Endorsed" -> return Endorsed
    "Abstained" -> return Abstained
    t -> fail ("expected either \"Endorsed\" or \"Abstained\"; got " ++ Text.unpack t)

data ModEndorsement = ModEndorsement
  { endorseStatus :: EndorsementStatus,
    timestamp :: POSIXTime
    -- TODO The objects from the server also have a "version" field,
    -- but it seems to always be null.  Is it?
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''ModEndorsement

data Mod = Mod
  { status :: ModStatus,
    uid :: Int,
    modId :: Int,
    gameId :: Int,
    allowRating :: Bool,
    domainName :: String,
    categoryId :: Int,
    version :: String,
    endorsementCount :: Int,
    -- NOTE The following two fields also have corresponding
    -- "timestamp" fields; these express the exact same information,
    -- but as POSIX timestamps.  We ignore them.
    createdTime :: UTCTime,
    updatedTime :: UTCTime,
    author :: String,
    uploadedBy :: String,
    uploadedUsersProfileUrl :: String,
    containsAdultContent :: Bool,
    available :: Bool,
    user :: ModUser,
    endorsement :: Maybe ModEndorsement
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON Mod where
  parseJSON = withObject "Mod" \v -> do
    status <-
      (v .: "status" :: Parser Text) >>= \case
        "not_published" -> return NotPublished
        "hidden" -> return Hidden
        -- If status is "published", then `v` should have all the
        -- PublishedModInfo fields.
        "published" -> Published <$> parseJSON (Object v)
        t -> fail ("expected one of \"not_published\", \"published\", or \"hidden\"; got " ++ Text.unpack t)
    Mod status
      <$> (v .: "uid")
      <*> (v .: "mod_id")
      <*> (v .: "game_id")
      <*> (v .: "allow_rating")
      <*> (v .: "domain_name")
      <*> (v .: "category_id")
      <*> (v .: "version")
      <*> (v .: "endorsement_count")
      <*> (v .: "created_time")
      <*> (v .: "updated_time")
      <*> (v .: "author")
      <*> (v .: "uploaded_by")
      <*> (v .: "uploaded_users_profile_url")
      <*> (v .: "contains_adult_content")
      <*> (v .: "available")
      <*> (v .: "user")
      <*> (v .: "endorsement")

data FileCategory = Main | Update | Optional | OldVersion | Miscellaneous
  deriving (Eq, Ord, Enum, Bounded, Read, Show)

instance FromJSON FileCategory where
  parseJSON = withText "FileCategory" \t -> case Text.toLower t of
    "main" -> return Main
    "update" -> return Update
    "optional" -> return Optional
    "old_version" -> return OldVersion
    "miscellaneous" -> return Miscellaneous
    _ -> fail ("expected a file category; got " ++ Text.unpack t)

-- XXX This requires FlexibleInstances, and I think it's kind of
-- suspect.  Might be better to use an internal newtype wrapper.
instance ToHttpApiData [FileCategory] where
  toQueryParam = Text.intercalate "," . map toText
   where
    toText Main = "main"
    toText Update = "update"
    toText Optional = "optional"
    toText OldVersion = "old_version"
    toText Miscellaneous = "miscellaneous"

data FileUpdate = FileUpdate
  { oldFileId :: Int,
    newFileId :: Int,
    oldFileName :: String,
    newFileName :: String,
    -- NOTE Omitted `uploaded_timestamp`.
    uploadedTime :: UTCTime
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''FileUpdate

data FileDetails = FileDetails
  { -- NOTE Omitted `id`.  As far as I can tell, this field is always
    -- a two-element array [fileId, 100].
    uid :: Int,
    fileId :: Int,
    name :: String,
    version :: String,
    categoryId :: Int,
    categoryName :: FileCategory,
    isPrimary :: Bool,
    size :: Int,
    fileName :: String,
    modVersion :: String,
    -- NOTE Omitted `uploaded_timestamp`.
    uploaded_time :: UTCTime,
    externalVirusScanUrl :: String,
    description :: String,
    sizeKb :: Int,
    sizeInBytes :: Int,
    changelog_html :: String,
    content_preview_link :: String
    -- NOTE Objects from the `{md5_hash}.json` endpoint also have an
    -- `md5` field, which contains the file's MD5 hash.  We omit it,
    -- because 1. then we can use this type at multiple endpoints, and
    -- 2. if you've found a file via its MD5 hash, you don't need to
    -- be told it.
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''FileDetails

data ModFiles = ModFiles
  { files :: [FileDetails],
    fileUpdates :: [FileUpdate]
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''ModFiles

data MD5Lookup = MD5Lookup
  { mod :: Mod,
    fileDetails :: FileDetails
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''MD5Lookup

newtype EndorseVersion = EndorseVersion
  { version :: Maybe String
  }
  deriving (Eq, Ord, Read, Show)

instance ToForm EndorseVersion where
  toForm v = version (v :: EndorseVersion) & fmap (\v -> ("version", [Text.pack v])) & toList & HashMap.fromList & Form

newtype DownloadExpiry = DownloadExpiry POSIXTime
  deriving (Eq, Ord, Read, Show)

instance ToHttpApiData DownloadExpiry where
  toQueryParam (DownloadExpiry t) = Text.pack . show . round @_ @Integer $ t

data DownloadLink = DownloadLink
  { name :: String,
    shortName :: String,
    uri :: String
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON DownloadLink where
  parseJSON = withObject "DownloadLink" \v ->
    DownloadLink <$> (v .: "name") <*> (v .: "short_name") <*> (v .: "URI")

data Category' = Category'
  { categoryId :: Int,
    name :: String,
    parentCategory :: Maybe Int
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON Category' where
  -- The @Category'@ type is internal; it exists only for the purposes
  -- of converting @[Category']@ into @[Category]@.  Therefore, we use
  -- the name @"Category"@ here.
  parseJSON = withObject "Category" \v -> do
    categoryId <- v .: "category_id"
    name <- v .: "name"
    parentCategory <-
      v .: "parent_category" >>= \case
        Bool False -> return Nothing
        x -> Just <$> parseJSON x <?> Key "name"
    return (Category' categoryId name parentCategory)

data Category = Category
  { categoryId :: Int,
    name :: String,
    parentCategory :: Maybe Category
  }
  deriving (Eq, Ord, Read, Show)

-- | Stitch a list of @Category'@ together to form a list of
-- @Category@.  Each @parentCategory@ becomes a reference to some
-- other category in the returned list.
stitchCategories :: [Category'] -> [Category]
stitchCategories cs = result
 where
  result =
    map
      ( \c ->
          Category
            { categoryId = categoryId (c :: Category'),
              name = name (c :: Category'),
              -- TODO What if the parent category isn't present?
              -- Instead of using `fromJust`, think about how to deal
              -- with that case.
              parentCategory =
                parentCategory (c :: Category') <&> \id ->
                  fromJust (find (\c' -> categoryId (c' :: Category) == id) result)
            }
      )
      cs

data Game = Game
  { id :: Int,
    name :: String,
    forumUrl :: String,
    nexusmodsUrl :: String,
    -- TODO Is this closed?  Should it be an ADT?
    genre :: String,
    fileCount :: Int,
    downloads :: Int,
    domainName :: String,
    approvedDate :: Int,
    fileViews :: Int,
    authors :: Int,
    fileEndorsements :: Int,
    mods :: Int,
    categories :: [Category]
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON Game where
  parseJSON = withObject "Game" \v ->
    Game
      <$> (v .: "id")
      <*> (v .: "name")
      <*> (v .: "forum_url")
      <*> (v .: "nexusmods_url")
      <*> (v .: "genre")
      <*> (v .: "file_count")
      <*> (v .: "downloads")
      <*> (v .: "domain_name")
      <*> (v .: "approved_date")
      <*> (v .: "file_views")
      <*> (v .: "authors")
      <*> (v .: "file_endorsements")
      <*> (v .: "mods")
      <*> (v .: "categories" <&> stitchCategories)

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

data Endorsement = Endorsement
  { modId :: Int,
    domainName :: String,
    date :: UTCTime,
    -- TODO The objects from the server also have a "version" field,
    -- but it seems to always be null.  Is it?
    status :: EndorsementStatus
  }
  deriving (Eq, Ord, Read, Show)

deriveFromJSON deriveJSONOptions ''Endorsement

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
      -- This can only occur if there is a bug in our @colour@ parser.
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
              -- This can only occur if there is a bug in
              -- @getHexDigit@.
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
  "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "updated.json" :> QueryParam' '[Required] "period" Period :> Header' '[Required] "apikey" String :> Get '[JSON] [ModUpdate]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> Capture "mod_id" Int :> "changelogs.json" :> Header' '[Required] "apikey" String :> Get '[JSON] Changelogs
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "latest_added.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Mod]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "latest_updated.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Mod]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "trending.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Mod]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> Capture "id" String :> Header' '[Required] "apikey" String :> Get '[JSON] Mod
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "md5_search" :> Capture "md5_hash" String :> Header' '[Required] "apikey" String :> Get '[JSON] [MD5Lookup]
    :<|> ( "v1"
            :> "games"
            :> Capture "game_domain_name" String
            :> "mods"
            :> Capture "id" Int
            :> "endorse.json"
            :> ReqBody '[FormUrlEncoded] EndorseVersion
            :> Header' '[Required] "apikey" String
            :> Post '[JSON] Message
         )
    :<|> ( "v1"
            :> "games"
            :> Capture "game_domain_name" String
            :> "mods"
            :> Capture "id" Int
            :> "abstain.json"
            :> ReqBody '[FormUrlEncoded] EndorseVersion
            :> Header' '[Required] "apikey" String
            :> Post '[JSON] Message
         )
    :<|> ( "v1"
            :> "games"
            :> Capture "game_domain_name" String
            :> "mods"
            :> Capture "mod_id" Int
            :> "files.json"
            :> QueryParam "category" [FileCategory]
            :> Header' '[Required] "apikey" String
            :> Get '[JSON] ModFiles
         )
    :<|> ( "v1"
            :> "games"
            :> Capture "game_domain_name" String
            :> "mods"
            :> Capture "mod_id" Int
            :> "files"
            :> Capture "file_id" String
            :> Header' '[Required] "apikey" String
            :> Get '[JSON] FileDetails
         )
    :<|> ( "v1"
            :> "games"
            :> Capture "game_domain_name" String
            :> "mods"
            :> Capture "mod_id" Int
            :> "files"
            :> Capture "id" Int
            :> "download_link.json"
            :> QueryParam "key" String
            :> QueryParam "expires" DownloadExpiry
            :> Header' '[Required] "apikey" String
            :> Get '[JSON] [DownloadLink]
         )
    :<|> "v1" :> "games.json" :> Header "include_unapproved" Bool :> Header' '[Required] "apikey" String :> Get '[JSON] [Game]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> Header' '[Required] "apikey" String :> Get '[JSON] Game
    :<|> "v1" :> "users" :> "validate.json" :> Header' '[Required] "apikey" String :> Get '[JSON] User
    :<|> "v1" :> "user" :> "tracked_mods.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [ModRef]
    :<|> ( "v1" :> "user" :> "tracked_mods.json"
            :> QueryParam' '[Required] "domain_name" String
            :> QueryParam' '[Required] "mod_id" Int
            :> Header' '[Required] "apikey" String
            :> UVerb 'POST '[JSON] '[WithStatus 200 Message, WithStatus 201 Message]
         )
    :<|> ( "v1" :> "user" :> "tracked_mods.json"
            :> QueryParam' '[Required] "domain_name" String
            :> QueryParam' '[Required] "mod_id" Int
            :> Header' '[Required] "apikey" String
            :> UVerb 'DELETE '[JSON] [WithStatus 200 Message, WithStatus 404 Message]
         )
    :<|> "v1" :> "user" :> "endorsements.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Endorsement]
    :<|> "v1" :> "colourschemes" :> Header' '[Required] "apikey" String :> Get '[JSON] [ColourScheme]

api :: Proxy NexusModsAPI
api = Proxy

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
