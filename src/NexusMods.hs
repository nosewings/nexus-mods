module NexusMods (
  Period (..),
  ModUpdate (..),
  Changelogs,
  PublishedModInfo (..),
  ModStatus (..),
  ModUser (..),
  ModEndorsement (..),
  Mod (..),
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
  getGames,
  getGame,
  validate,
  getTrackedMods,
  trackMod,
  untrackMod,
  getColourSchemes,
  runNexus,
) where

import Data.Aeson
import Data.Aeson.Internal (JSONPathElement (..))
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.Data
import Data.Foldable
import Data.Functor
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

data Category' = Category'
  { categoryId :: Int,
    name :: String,
    parentCategory :: Maybe Int
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON Category' where
  -- NOTE The @Category'@ type is internal; it exists only for the
  -- purposes of converting @[Category']@ into @[Category]@.
  -- Therefore, we use the name @"Category"@ here.
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
  "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "updated.json" :> Header' '[Required] "apikey" String :> QueryParam' '[Required] "period" Period :> Get '[JSON] [ModUpdate]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> Capture "mod_id" Int :> "changelogs.json" :> Header' '[Required] "apikey" String :> Get '[JSON] Changelogs
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "latest_added.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Mod]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "latest_updated.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Mod]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> "trending.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Mod]
    :<|> "v1" :> "games" :> Capture "game_domain_name" String :> "mods" :> Capture "id" String :> Header' '[Required] "apikey" String :> Get '[JSON] Mod
    :<|> "v1" :> "games.json" :> Header' '[Required] "apikey" String :> Header "include_unapproved" Bool :> Get '[JSON] [Game]
    :<|> "v1" :> "games" :> Header' '[Required] "apikey" String :> Capture "game_domain_name" String :> Get '[JSON] Game
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
    :<|> "v1" :> "user" :> "endorsements.json" :> Header' '[Required] "apikey" String :> Get '[JSON] [Endorsement]
    :<|> "v1" :> "colourschemes" :> Header' '[Required] "apikey" String :> Get '[JSON] [ColourScheme]

api :: Proxy NexusModsAPI
api = Proxy

-- | Get a list of mod updates within a given time period.
getUpdates :: String -> String -> Period -> ClientM [ModUpdate]

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

-- | Get all games.
getGames :: String -> Maybe Bool -> ClientM [Game]

-- | Internal version of @getGame@.  This is necessary because the API
-- path is @/v1/games/{game_domain_name}.json@, but Servant does not
-- (as far as I know) give us the ability to modify a captured path
-- component.
getGame' :: String -> String -> ClientM Game

-- | Get a specific game.
getGame :: String -> String -> ClientM Game
getGame key gameDomainName = getGame' key (gameDomainName ++ ".json")

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
  :<|> getGames
  :<|> getGame'
  :<|> validate
  :<|> getTrackedMods
  :<|> trackMod'
  :<|> untrackMod'
  :<|> getEndorsements
  :<|> getColourSchemes = client api

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
