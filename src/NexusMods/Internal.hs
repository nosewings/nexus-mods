module NexusMods.Internal (
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
  DownloadExpiry (..),
  DownloadLink (..),
  Message (..),
  EndorseVersion (..),
  User (..),
  Endorsement (..),
  ModRef (..),
  Colour (..),
  ColourScheme (..),
  NexusModsAPI,
  api,
) where

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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import GHC.Generics
import NexusMods.Indexed qualified as Indexed
import NexusMods.Surgery
import NexusMods.TH
import Servant.API
import Text.ParserCombinators.ReadP
import Web.FormUrlEncoded

impossible :: a
impossible = error "an impossible situation has occurred"

data Category' = Category'
  { categoryId :: Int,
    name :: String,
    parentCategory :: Maybe Int
  }
  deriving (Eq, Ord, Read, Show, Generic)

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
  deriving (Eq, Ord, Read, Show, Generic)

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
  deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON Game where
  parseJSON v = genericParseJSON deriveJSONOptions v <&> animate (modifyRField @"categories" stitchCategories)

data Period = Day | Week | Month
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance ToHttpApiData Period where
  toQueryParam Day = "1d"
  toQueryParam Week = "1w"
  toQueryParam Month = "1m"

data ModUpdate = ModUpdate
  { modId :: Int,
    latestFileUpdate :: UTCTime,
    latestModActivity :: UTCTime
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON ModUpdate where
  parseJSON v =
    genericParseJSON deriveJSONOptions v <&> animate Indexed.do
      modifyRField @"latestFileUpdate" posixSecondsToUTCTime
      modifyRField @"latestModActivity" posixSecondsToUTCTime

data PublishedModInfo = PublishedModInfo
  { name :: String,
    summary :: String,
    description :: String,
    pictureUrl :: String,
    modDownloads :: Int,
    modUniqueDownloads :: Int
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''PublishedModInfo

data ModStatus
  = NotPublished
  | Published PublishedModInfo
  | Hidden
  deriving (Eq, Ord, Read, Show, Generic)

data ModUser = ModUser
  { memberId :: Int,
    memberGroupId :: Int,
    name :: String
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''ModUser

-- TODO Is there anything else?
data EndorsementStatus = Endorsed | Abstained
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

instance FromJSON EndorsementStatus where
  parseJSON = withText "EndorsementStatus" \case
    "Endorsed" -> return Endorsed
    "Abstained" -> return Abstained
    t -> fail ("expected either \"Endorsed\" or \"Abstained\"; got " ++ Text.unpack t)

data ModEndorsement = ModEndorsement
  { endorseStatus :: EndorsementStatus,
    time :: UTCTime
    -- TODO The objects from the server also have a "version" field,
    -- but it seems to always be null.  Is it?
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON ModEndorsement where
  parseJSON v =
    genericParseJSON deriveJSONOptions v <&> animate Indexed.do
      t <- removeRField @"timestamp" @1
      insertRField' @"time" (posixSecondsToUTCTime t)

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
  deriving (Eq, Ord, Read, Show, Generic)

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
      <$> v .: "uid"
      <*> v .: "mod_id"
      <*> v .: "game_id"
      <*> v .: "allow_rating"
      <*> v .: "domain_name"
      <*> v .: "category_id"
      <*> v .: "version"
      <*> v .: "endorsement_count"
      <*> v .: "created_time"
      <*> v .: "updated_time"
      <*> v .: "author"
      <*> v .: "uploaded_by"
      <*> v .: "uploaded_users_profile_url"
      <*> v .: "contains_adult_content"
      <*> v .: "available"
      <*> v .: "user"
      <*> v .: "endorsement"

data FileCategory = Main | Update | Optional | OldVersion | Miscellaneous
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

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
    uploadedTime :: UTCTime,
    externalVirusScanUrl :: String,
    description :: String,
    sizeKb :: Int,
    sizeInBytes :: Int,
    changelogHtml :: String,
    contentPreviewLink :: String
    -- NOTE Objects from the `{md5_hash}.json` endpoint also have an
    -- `md5` field, which contains the file's MD5 hash.  We omit it,
    -- because 1. then we can use this type at multiple endpoints, and
    -- 2. if you've found a file via its MD5 hash, you don't need to
    -- be told it.
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''FileDetails

data MD5Lookup = MD5Lookup
  { mod :: Mod,
    fileDetails :: FileDetails
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''MD5Lookup

-- | A list of mod changelogs.
type Changelogs = Map String [String]

data FileUpdate = FileUpdate
  { oldFileId :: Int,
    newFileId :: Int,
    oldFileName :: String,
    newFileName :: String,
    -- NOTE Omitted `uploaded_timestamp`.
    uploadedTime :: UTCTime
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''FileUpdate

data ModFiles = ModFiles
  { files :: [FileDetails],
    fileUpdates :: [FileUpdate]
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''ModFiles

newtype DownloadExpiry = DownloadExpiry POSIXTime
  deriving (Eq, Ord, Read, Show, Generic)

instance ToHttpApiData DownloadExpiry where
  toQueryParam (DownloadExpiry t) = Text.pack . show . round @_ @Integer $ t

data DownloadLink = DownloadLink
  { name :: String,
    shortName :: String,
    uri :: String
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance FromJSON DownloadLink where
  parseJSON v =
    genericParseJSON deriveJSONOptions v <&> animate Indexed.do
      t <- removeRField @"URI" @2
      insertRField' @"uri" t

newtype EndorseVersion = EndorseVersion
  { version :: Maybe String
  }
  deriving (Eq, Ord, Read, Show, Generic)

instance ToForm EndorseVersion where
  toForm v = version (v :: EndorseVersion) & fmap (\v -> ("version", [Text.pack v])) & toList & HashMap.fromList & Form

-- | Details about a Nexus Mods user.
data User = User
  { userId :: Int,
    key :: String,
    email :: String,
    profileUrl :: String,
    isPremium :: Bool,
    isSupporter :: Bool
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''User

data Endorsement = Endorsement
  { modId :: Int,
    domainName :: String,
    date :: UTCTime,
    -- TODO The objects from the server also have a "version" field,
    -- but it seems to always be null.  Is it?
    status :: EndorsementStatus
  }
  deriving (Eq, Ord, Read, Show, Generic)

data ModRef = ModRef
  { modId :: Int,
    domainName :: String
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''ModRef

deriveFromJSON deriveJSONOptions ''Endorsement

newtype Message = Message
  { message :: String
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''Message

data Colour = Colour
  { red :: Word8,
    blue :: Word8,
    green :: Word8
  }
  deriving (Eq, Ord, Read, Show, Generic)

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
              -- @isHexDigit@.
              | otherwise -> impossible

data ColourScheme = ColourScheme
  { id :: Int,
    name :: String,
    primaryColour :: Colour,
    secondaryColour :: Colour,
    darkerColour :: Colour
  }
  deriving (Eq, Ord, Read, Show, Generic)

deriveFromJSON deriveJSONOptions ''ColourScheme

type NexusModsAPI =
  Header' '[Required] "apikey" String :> "v1"
    :> ( "games.json" :> Header "include_unapproved" Bool :> Get '[JSON] [Game]
          :<|> "games"
          :> Capture "game_domain_name" String
          :> ( Get '[JSON] Game
                :<|> "mods"
                  :> ( "updated.json" :> QueryParam' '[Required] "period" Period :> Get '[JSON] [ModUpdate]
                        :<|> "latest_added.json" :> Get '[JSON] [Mod]
                        :<|> "latest_updated.json" :> Get '[JSON] [Mod]
                        :<|> "trending.json" :> Get '[JSON] [Mod]
                        :<|> "md5_search" :> Capture "md5_hash" String :> Get '[JSON] [MD5Lookup]
                        :<|> Capture "id" String :> Get '[JSON] Mod
                        :<|> Capture "mod_id" Int
                          :> ( "changelogs.json" :> Get '[JSON] Changelogs
                                :<|> "files.json" :> QueryParam "category" [FileCategory] :> Get '[JSON] ModFiles
                                :<|> "files"
                                  :> ( Capture "file_id" String :> Get '[JSON] FileDetails
                                        :<|> Capture "id" Int :> "download_link.json" :> QueryParam "key" String :> QueryParam "expires" DownloadExpiry :> Get '[JSON] [DownloadLink]
                                     )
                                :<|> ReqBody '[FormUrlEncoded] EndorseVersion
                                  :> ( "endorse.json" :> Post '[JSON] Message
                                        :<|> "abstain.json" :> Post '[JSON] Message
                                     )
                             )
                     )
             )
          :<|> "user"
          :> ( "validate.json" :> Get '[JSON] User
                :<|> ("endorsements.json" :> Get '[JSON] [Endorsement])
                :<|> "tracked_mods.json"
                :> ( Get '[JSON] [ModRef]
                      :<|> QueryParam' '[Required] "domain_name" String :> QueryParam' '[Required] "mod_id" Int
                        :> ( UVerb 'POST '[JSON] '[WithStatus 200 Message, WithStatus 201 Message]
                              :<|> UVerb 'DELETE '[JSON] [WithStatus 200 Message, WithStatus 404 Message]
                           )
                   )
             )
          :<|> ("colourschemes" :> Get '[JSON] [ColourScheme])
       )

api :: Proxy NexusModsAPI
api = Proxy
