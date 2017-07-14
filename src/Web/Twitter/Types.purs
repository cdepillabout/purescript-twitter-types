module Web.Twitter.Types
       -- ( UserId
       -- , Friends
       -- , URIString
       -- , UserName
       -- , StatusId
       -- , LanguageCode
       -- , StreamingAPI(..)
       -- , Status(..)
       -- , SearchResult(..)
       -- , SearchStatus(..)
       -- , SearchMetadata(..)
       -- , RetweetedStatus(..)
       -- , DirectMessage(..)
       -- , EventTarget(..)
       -- , Event(..)
       -- , Delete(..)
       -- , User(..)
       -- , List'(..)
       -- , Entities(..)
       -- , EntityIndices
       -- , Entity(..)
       -- , HashTagEntity(..)
       -- , UserEntity(..)
       -- , URLEntity(..)
       -- , MediaEntity(..)
       -- , MediaSize(..)
       -- , Coordinates(..)
       -- , Place(..)
       -- , BoundingBox(..)
       -- , Contributor(..)
       -- , UploadedMedia (..)
       -- , ImageSizeType (..)
       -- , checkError
       -- , twitterTimeFormat
       -- )
       where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut
  (class DecodeJson, class EncodeJson, JNumber, JObject, Json, (.?),
   (:=), (~>), decodeJson, encodeJson, foldJson, foldJsonObject,
   foldJsonString, fromObject, fromString, jsonEmptyObject, toObject)
import Data.Argonaut.Decode.Combinators ((.??), (.?=))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.StrMap (StrMap, insert)

type UserId = Int
type Friends = Array UserId
type URIString = String
type UserName = String
type StatusId = Int
type LanguageCode = String


checkError :: JObject -> Either String Unit
checkError o = do
  err <- o .?? "error"
  go err
  where
    go :: Maybe String -> Either String Unit
    go (Just msg) = Left msg
    go Nothing = Right unit

checkError'
  :: forall a. (JObject -> Either String a) -> JObject -> Either String a
checkError' realFunc o =
  checkError o *> realFunc o

-- | This is a time format displayed in a specific way.  See the function
-- | `twitterTimeFormat` for an example of this.
-- |
-- | This needs to actually be implemented.
newtype TwitterTime = TwitterTime DateTime
--newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }

fromTwitterTime :: TwitterTime -> DateTime
fromTwitterTime (TwitterTime dateTime) = dateTime

-- | Example:
-- | Sat Jun 14 10:15:19 +0000 2014
twitterTimeFormat :: String
twitterTimeFormat = "%a %b %d %T %z %Y"

derive instance genericTwitterTime :: Generic TwitterTime _
derive instance newtypeTwitterTime :: Newtype TwitterTime _
derive instance eqTwitterTime :: Eq TwitterTime
instance showTwitterTime :: Show TwitterTime where show = genericShow

--instance FromJSON TwitterTime where
--    parseJSON = withText "TwitterTime" $ \t ->
--#if MIN_VERSION_time(1, 5, 0)
--        case parseTimeM True defaultTimeLocale twitterTimeFormat (unpack t) of
--#else
--        case parseTime defaultTimeLocale twitterTimeFormat (unpack t) of
--#endif
--            Just  d -> pure $ TwitterTime d
--            Nothing -> fail $ "Could not parse twitter time. Text was: " ++ unpack t

--instance ToJSON TwitterTime where
--    toJSON t = String $ pack $ formatTime defaultTimeLocale twitterTimeFormat $ fromTwitterTime t



newtype HackyWrapperShouldBeTwitterTime = HackyWrapperShouldBeTwitterTime HackyDateTime
--newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }

fromHackyWrapperShouldBeTwitterTime :: HackyWrapperShouldBeTwitterTime -> HackyDateTime
fromHackyWrapperShouldBeTwitterTime (HackyWrapperShouldBeTwitterTime hackyDateTime) =
  hackyDateTime

derive instance genericHackyWrapperShouldBeTwitterTime :: Generic HackyWrapperShouldBeTwitterTime _
derive instance newtypeHackyWrapperShouldBeTwitterTime :: Newtype HackyWrapperShouldBeTwitterTime _
derive instance eqHackyWrapperShouldBeTwitterTime :: Eq HackyWrapperShouldBeTwitterTime
instance showHackyWrapperShouldBeTwitterTime :: Show HackyWrapperShouldBeTwitterTime where show = genericShow


instance decodeJsonHackyWrapperShouldBeTwitterTime :: DecodeJson HackyWrapperShouldBeTwitterTime where
  decodeJson :: Json -> Either String HackyWrapperShouldBeTwitterTime
  decodeJson =
    foldJsonString (Left "not a JString (HackyWrapperShouldBeTwitterTime)") \s ->
      Right $ HackyWrapperShouldBeTwitterTime $ HackyDateTime s

instance encodeJsonHackyWrapperShouldBeTwitterTime :: EncodeJson HackyWrapperShouldBeTwitterTime where
  encodeJson :: HackyWrapperShouldBeTwitterTime -> Json
  encodeJson (HackyWrapperShouldBeTwitterTime (HackyDateTime s)) = fromString s



-- | This should be used in place of UTCTime that is being converted from a TwitterTime.
newtype HackyDateTime = HackyDateTime String

fromHackyDateTime :: HackyDateTime -> String
fromHackyDateTime (HackyDateTime s) = s

derive instance genericHackyDateTime :: Generic HackyDateTime _
derive instance newtypeHackyDateTime :: Newtype HackyDateTime _
derive instance eqHackyDateTime :: Eq HackyDateTime
instance showHackyDateTime :: Show HackyDateTime where show = genericShow

data StreamingAPI
  = SStatus Status
  | SRetweetedStatus RetweetedStatus
  | SEvent Event
  | SDelete Delete
  | SFriends Friends
  | SDirectMessage DirectMessage
  | SUnknown Json

derive instance genericStreamingAPI :: Generic StreamingAPI _
derive instance eqStreamingAPI :: Eq StreamingAPI
instance showStreamingAPI :: Show StreamingAPI where show = genericShow

instance decodeJsonStreamingAPI :: DecodeJson StreamingAPI where
  decodeJson :: Json -> Either String StreamingAPI
  decodeJson =
    foldJsonObject (Left "not a JObject (StreamingAPI)") \o ->
      let json = fromObject o
      in
        SRetweetedStatus <$> decodeJson json <|>
        SStatus <$> decodeJson json <|>
        SEvent <$> decodeJson json <|>
        SDelete <$> decodeJson json <|>
        SFriends <$> (o .? "friends") <|>
        SDirectMessage <$> (o .? "direct_message") <|>
        pure (SUnknown json)

instance encodeJsonStreamingAPI :: EncodeJson StreamingAPI where
  encodeJson :: StreamingAPI -> Json
  encodeJson (SRetweetedStatus retweetedStatus) = encodeJson retweetedStatus
  encodeJson (SStatus status) = encodeJson status
  encodeJson (SEvent event) = encodeJson event
  encodeJson (SDelete delete) = encodeJson delete
  encodeJson (SFriends friends) = encodeJson friends
  encodeJson (SDirectMessage directMessage) = encodeJson directMessage
  encodeJson (SUnknown unknown) = unknown

-- | This type represents a Twitter tweet structure.
-- See <https://dev.twitter.com/docs/platform-objects/tweets>.
newtype Status = Status
  { statusContributors :: Maybe (Array Contributor)
  , statusCoordinates :: Maybe Coordinates
  , statusCreatedAt :: HackyDateTime
  , statusCurrentUserRetweet :: Maybe StatusId
  , statusEntities :: Maybe Entities
  , statusExtendedEntities :: Maybe Entities
  , statusFavoriteCount :: Int
  , statusFavorited :: Maybe Boolean
  , statusFilterLevel :: Maybe String
  , statusId :: StatusId
  , statusInReplyToScreenName :: Maybe String
  , statusInReplyToStatusId :: Maybe StatusId
  , statusInReplyToUserId :: Maybe UserId
  , statusLang :: Maybe LanguageCode
  , statusPlace :: Maybe Place
  , statusPossiblySensitive :: Maybe Boolean
  , statusScopes :: Maybe JObject
  , statusQuotedStatusId :: Maybe StatusId
  , statusQuotedStatus :: Maybe Status
  , statusRetweetCount :: Int
  , statusRetweeted :: Maybe Boolean
  , statusRetweetedStatus :: Maybe Status
  , statusSource :: String
  , statusText :: String
  , statusTruncated :: Boolean
  , statusUser :: User
  , statusWithheldCopyright :: Maybe Boolean
  , statusWithheldInCountries :: Maybe (Array String)
  , statusWithheldScope :: Maybe String
  }

toStatus
  :: Maybe (Array Contributor)
  -> Maybe Coordinates
  -> HackyDateTime
  -> Maybe StatusId
  -> Maybe Entities
  -> Maybe Entities
  -> Int
  -> Maybe Boolean
  -> Maybe String
  -> StatusId
  -> Maybe String
  -> Maybe StatusId
  -> Maybe UserId
  -> Maybe LanguageCode
  -> Maybe Place
  -> Maybe Boolean
  -> Maybe JObject
  -> Maybe StatusId
  -> Maybe Status
  -> Int
  -> Maybe Boolean
  -> Maybe Status
  -> String
  -> String
  -> Boolean
  -> User
  -> Maybe Boolean
  -> Maybe (Array String)
  -> Maybe String
  -> Status
toStatus
    statusContributors statusCoordinates statusCreatedAt
    statusCurrentUserRetweet statusEntities statusExtendedEntities
    statusFavoriteCount statusFavorited statusFilterLevel statusId
    statusInReplyToScreenName statusInReplyToStatusId statusInReplyToUserId
    statusLang statusPlace statusPossiblySensitive statusScopes
    statusQuotedStatusId statusQuotedStatus statusRetweetCount statusRetweeted
    statusRetweetedStatus statusSource statusText statusTruncated statusUser
    statusWithheldCopyright statusWithheldInCountries statusWithheldScope =
  Status
    { statusContributors
    , statusCoordinates
    , statusCreatedAt
    , statusCurrentUserRetweet
    , statusEntities
    , statusExtendedEntities
    , statusFavoriteCount
    , statusFavorited
    , statusFilterLevel
    , statusId
    , statusInReplyToScreenName
    , statusInReplyToStatusId
    , statusInReplyToUserId
    , statusLang
    , statusPlace
    , statusPossiblySensitive
    , statusScopes
    , statusQuotedStatusId
    , statusQuotedStatus
    , statusRetweetCount
    , statusRetweeted
    , statusRetweetedStatus
    , statusSource
    , statusText
    , statusTruncated
    , statusUser
    , statusWithheldCopyright
    , statusWithheldInCountries
    , statusWithheldScope
    }

derive instance genericStatus :: Generic Status _
derive instance newtypeStatus :: Newtype Status _
derive instance eqStatus :: Eq Status
instance showStatus :: Show Status where show x = genericShow x

instance decodeJsonStatus :: DecodeJson Status where
  decodeJson :: Json -> Either String Status
  decodeJson =
    foldJsonObject (Left "not a JObject (Status)") $ checkError' \o ->
      toStatus
        <$> o .?? "contributors"
        <*> o .?? "coordinates"
        <*> map fromHackyWrapperShouldBeTwitterTime (o .?  "created_at")
        <*> (((o .? "current_user_retweet") >>= (\oo -> oo .? "id")) <|> pure Nothing)
        <*> o .?? "entities"
        <*> o .?? "extended_entities"
        <*> o .?? "favorite_count" .?= 0
        <*> o .?? "favorited"
        <*> o .?? "filter_level"
        <*> o .?  "id"
        <*> o .?? "in_reply_to_screen_name"
        <*> o .?? "in_reply_to_status_id"
        <*> o .?? "in_reply_to_user_id"
        <*> o .?? "lang"
        <*> o .?? "place"
        <*> o .?? "possibly_sensitive"
        <*> o .?? "scopes"
        <*> o .?? "quoted_status_id"
        <*> o .?? "quoted_status"
        <*> o .?? "retweet_count" .?= 0
        <*> o .?? "retweeted"
        <*> o .?? "retweeted_status"
        <*> o .?  "source"
        <*> o .?  "text"
        <*> o .?  "truncated"
        <*> o .?  "user"
        <*> o .?? "withheld_copyright"
        <*> o .?? "withheld_in_countries"
        <*> o .?? "withheld_scope"

instance encodeJsonStatus :: EncodeJson Status where
  encodeJson :: Status -> Json
  encodeJson (Status status) =
    "contributors" := status.statusContributors ~>
    "coordinates" := status.statusCoordinates ~>
    "created_at" := HackyWrapperShouldBeTwitterTime status.statusCreatedAt ~>
    "current_user_retweet" :=
      ( "id" := status.statusCurrentUserRetweet ~>
        "id_str" := show status.statusCurrentUserRetweet ~>
        jsonEmptyObject
      ) ~>
    "entities" := status.statusEntities ~>
    "extended_entities" := status.statusExtendedEntities ~>
    "favorite_count" := status.statusFavoriteCount ~>
    "favorited" := status.statusFavorited ~>
    "filter_level" := status.statusFilterLevel ~>
    "id" := status.statusId ~>
    "in_reply_to_screen_name" := status.statusInReplyToScreenName ~>
    "in_reply_to_status_id" := status.statusInReplyToStatusId ~>
    "in_reply_to_user_id" := status.statusInReplyToUserId ~>
    "lang" := status.statusLang ~>
    "place" := status.statusPlace ~>
    "possibly_sensitive" := status.statusPossiblySensitive ~>
    "scopes" := status.statusScopes ~>
    "quoted_status_id" := status.statusQuotedStatusId ~>
    "quoted_status" := status.statusQuotedStatus ~>
    "retweet_count" := status.statusRetweetCount ~>
    "retweeted" := status.statusRetweeted ~>
    "retweeted_status" := status.statusRetweetedStatus ~>
    "source" := status.statusSource ~>
    "text" := status.statusText ~>
    "truncated" := status.statusTruncated ~>
    "user" := status.statusUser ~>
    "withheld_copyright" := status.statusWithheldCopyright ~>
    "withheld_in_countries" := status.statusWithheldInCountries ~>
    "withheld_scope" := status.statusWithheldScope ~>
    jsonEmptyObject

newtype SearchResult body = SearchResult
  { searchResultStatuses :: body
  , searchResultSearchMetadata :: SearchMetadata
  }

toSearchResult :: forall body. body -> SearchMetadata -> SearchResult body
toSearchResult searchResultStatuses searchResultSearchMetadata =
  SearchResult {searchResultStatuses, searchResultSearchMetadata}

derive instance genericSearchResult :: Generic (SearchResult body) _
derive instance newtypeSearchResult :: Newtype (SearchResult body) _
derive instance eqSearchResult :: Eq body => Eq (SearchResult body)
instance showSearchResult :: Show body => Show (SearchResult body) where
  show = genericShow

instance decodeJsonSearchResult
    :: DecodeJson body => DecodeJson (SearchResult body) where
  decodeJson :: Json -> Either String (SearchResult body)
  decodeJson =
    foldJsonObject (Left "not a JObject (SearchResult)") $ checkError' \o ->
      toSearchResult
        <$> o .? "statuses"
        <*> o .?  "search_metadata"

instance encodeJsonSearchResult
    :: EncodeJson body => EncodeJson (SearchResult body) where
  encodeJson :: SearchResult body -> Json
  encodeJson (SearchResult searchResult) =
    "statuses" := searchResult.searchResultStatuses ~>
    "search_metadata" := searchResult.searchResultSearchMetadata ~>
    jsonEmptyObject

newtype SearchStatus = SearchStatus
  { searchStatusCreatedAt :: HackyDateTime
  , searchStatusId :: StatusId
  , searchStatusText :: String
  , searchStatusSource :: String
  , searchStatusUser :: User
  , searchStatusCoordinates :: Maybe Coordinates
  }

toSearchStatus
  :: HackyDateTime
  -> StatusId
  -> String
  -> String
  -> User
  -> Maybe Coordinates
  -> SearchStatus
toSearchStatus
    searchStatusCreatedAt searchStatusId searchStatusText searchStatusSource
    searchStatusUser searchStatusCoordinates =
  SearchStatus
    { searchStatusCreatedAt
    , searchStatusId
    , searchStatusText
    , searchStatusSource
    , searchStatusUser
    , searchStatusCoordinates
    }

derive instance genericSearchStatus :: Generic SearchStatus _
derive instance newtypeSearchStatus :: Newtype SearchStatus _
derive instance eqSearchStatus :: Eq SearchStatus
instance showSearchStatus :: Show SearchStatus where show = genericShow

instance decodeJsonSearchStatus :: DecodeJson SearchStatus where
  decodeJson :: Json -> Either String SearchStatus
  decodeJson =
    foldJsonObject (Left "not a JObject (SearchStatus)") $ checkError' \o ->
      toSearchStatus
        <$> map fromHackyWrapperShouldBeTwitterTime (o .?  "created_at")
        <*> o .?  "id"
        <*> o .?  "text"
        <*> o .?  "source"
        <*> o .?  "user"
        <*> o .?? "coordinates"

instance encodeJsonSearchStatus :: EncodeJson SearchStatus where
  encodeJson :: SearchStatus -> Json
  encodeJson (SearchStatus searchStatus) =
    "created_at" := HackyWrapperShouldBeTwitterTime searchStatus.searchStatusCreatedAt ~>
    "id" := searchStatus.searchStatusId ~>
    "text" := searchStatus.searchStatusText ~>
    "source" := searchStatus.searchStatusSource ~>
    "user" := searchStatus.searchStatusUser ~>
    "coordinates" := searchStatus.searchStatusCoordinates ~>
    jsonEmptyObject

newtype SearchMetadata = SearchMetadata
  { searchMetadataMaxId :: StatusId
  , searchMetadataSinceId :: StatusId
  , searchMetadataRefreshURL :: URIString
  , searchMetadataNextResults :: Maybe URIString
  , searchMetadataCount :: Int
  , searchMetadataCompletedIn :: Maybe Number
  , searchMetadataSinceIdStr :: String
  , searchMetadataQuery :: String
  , searchMetadataMaxIdStr :: String
  }

toSearchMetadata
  :: StatusId
  -> StatusId
  -> URIString
  -> Maybe URIString
  -> Int
  -> Maybe Number
  -> String
  -> String
  -> String
  -> SearchMetadata
toSearchMetadata searchMetadataMaxId searchMetadataSinceId searchMetadataRefreshURL searchMetadataNextResults searchMetadataCount searchMetadataCompletedIn searchMetadataSinceIdStr searchMetadataQuery searchMetadataMaxIdStr =
  SearchMetadata
    { searchMetadataMaxId
    , searchMetadataSinceId
    , searchMetadataRefreshURL
    , searchMetadataNextResults
    , searchMetadataCount
    , searchMetadataCompletedIn
    , searchMetadataSinceIdStr
    , searchMetadataQuery
    , searchMetadataMaxIdStr
    }

derive instance genericSearchMetadata :: Generic SearchMetadata _
derive instance newtypeSearchMetadata :: Newtype SearchMetadata _
derive instance eqSearchMetadata :: Eq SearchMetadata
instance showSearchMetadata :: Show SearchMetadata where show = genericShow

instance decodeJsonSearchMetadata :: DecodeJson SearchMetadata where
  decodeJson :: Json -> Either String SearchMetadata
  decodeJson =
    foldJsonObject (Left "not a JObject (SearchMetadata)") $ checkError' \o ->
      toSearchMetadata
        <$> o .? "max_id"
        <*> o .? "since_id"
        <*> o .? "refresh_url"
        <*> o .?? "next_results"
        <*> o .? "count"
        <*> o .?? "completed_id"
        <*> o .? "since_id_str"
        <*> o .? "query"
        <*> o .? "max_id_str"

instance encodeJsonSearchMetadata :: EncodeJson SearchMetadata where
  encodeJson :: SearchMetadata -> Json
  encodeJson (SearchMetadata searchMetadata) =
    "max_id" := searchMetadata.searchMetadataMaxId ~>
    "since_id" := searchMetadata.searchMetadataSinceId ~>
    "refresh_url" := searchMetadata.searchMetadataRefreshURL ~>
    "next_results" := searchMetadata.searchMetadataNextResults ~>
    "count" := searchMetadata.searchMetadataCount ~>
    "completed_in" := searchMetadata.searchMetadataCompletedIn ~>
    "since_id_str" := searchMetadata.searchMetadataSinceIdStr ~>
    "query" := searchMetadata.searchMetadataQuery ~>
    "max_id_str" := searchMetadata.searchMetadataMaxIdStr ~>
    jsonEmptyObject

newtype RetweetedStatus = RetweetedStatus
  { rsCreatedAt :: HackyDateTime
  , rsId :: StatusId
  , rsText :: String
  , rsSource :: String
  , rsTruncated :: Boolean
  , rsEntities :: Maybe Entities
  , rsUser :: User
  , rsRetweetedStatus :: Status
  , rsCoordinates :: Maybe Coordinates
  }

toRetweetedStatus
  :: HackyDateTime
  -> StatusId
  -> String
  -> String
  -> Boolean
  -> Maybe Entities
  -> User
  -> Status
  -> Maybe Coordinates
  -> RetweetedStatus
toRetweetedStatus
    rsCreatedAt rsId rsText rsSource rsTruncated rsEntities rsUser
    rsRetweetedStatus rsCoordinates =
  RetweetedStatus
    { rsCreatedAt
    , rsId
    , rsText
    , rsSource
    , rsTruncated
    , rsEntities
    , rsUser
    , rsRetweetedStatus
    , rsCoordinates
    }

derive instance genericRetweetedStatus :: Generic RetweetedStatus _
derive instance newtypeRetweetedStatus :: Newtype RetweetedStatus _
derive instance eqRetweetedStatus :: Eq RetweetedStatus
instance showRetweetedStatus :: Show RetweetedStatus where show = genericShow

instance decodeJsonRetweetedStatus :: DecodeJson RetweetedStatus where
  decodeJson :: Json -> Either String RetweetedStatus
  decodeJson =
    foldJsonObject (Left "not a JObject (RetweetedStatus)") $ checkError' \o ->
      toRetweetedStatus
        <$> map fromHackyWrapperShouldBeTwitterTime (o .?  "created_at")
        <*> o .?  "id"
        <*> o .?  "text"
        <*> o .?  "source"
        <*> o .?  "truncated"
        <*> o .?? "entities"
        <*> o .?  "user"
        <*> o .?  "retweeted_status"
        <*> o .?? "coordinates"

instance encodeJsonRetweetedStatus :: EncodeJson RetweetedStatus where
  encodeJson :: RetweetedStatus -> Json
  encodeJson (RetweetedStatus retweetedStatus) =
    "created_at" := HackyWrapperShouldBeTwitterTime retweetedStatus.rsCreatedAt ~>
    "id" := retweetedStatus.rsId ~>
    "text" := retweetedStatus.rsText ~>
    "source" := retweetedStatus.rsSource ~>
    "truncated" := retweetedStatus.rsTruncated ~>
    "entities" := retweetedStatus.rsEntities ~>
    "user" := retweetedStatus.rsUser ~>
    "retweeted_status" := retweetedStatus.rsRetweetedStatus ~>
    "coordinates" := retweetedStatus.rsCoordinates ~>
    jsonEmptyObject

newtype DirectMessage = DirectMessage
  { dmCreatedAt :: HackyDateTime
  , dmSenderScreenName :: String
  , dmSender :: User
  , dmText :: String
  , dmRecipientScreeName :: String
  , dmId :: StatusId
  , dmRecipient :: User
  , dmRecipientId :: UserId
  , dmSenderId :: UserId
  , dmCoordinates :: Maybe Coordinates
  }

toDirectMessage
  :: HackyDateTime
  -> String
  -> User
  -> String
  -> String
  -> StatusId
  -> User
  -> UserId
  -> UserId
  -> Maybe Coordinates
  -> DirectMessage
toDirectMessage
    dmCreatedAt dmSenderScreenName dmSender dmText dmRecipientScreeName dmId
    dmRecipient dmRecipientId dmSenderId dmCoordinates =
  DirectMessage
    { dmCreatedAt
    , dmSenderScreenName
    , dmSender
    , dmText
    , dmRecipientScreeName
    , dmId
    , dmRecipient
    , dmRecipientId
    , dmSenderId
    , dmCoordinates
    }

derive instance genericDirectMessage :: Generic DirectMessage _
derive instance newtypeDirectMessage :: Newtype DirectMessage _
derive instance eqDirectMessage :: Eq DirectMessage
instance showDirectMessage :: Show DirectMessage where show = genericShow

instance decodeJsonDirectMessage :: DecodeJson DirectMessage where
  decodeJson :: Json -> Either String DirectMessage
  decodeJson =
    foldJsonObject (Left "not a JObject (DirectMessage)") \o ->
      toDirectMessage
        <$> map fromHackyWrapperShouldBeTwitterTime (o .?  "created_at")
        <*> o .?  "sender_screen_name"
        <*> o .?  "sender"
        <*> o .?  "text"
        <*> o .?  "recipient_screen_name"
        <*> o .?  "id"
        <*> o .?  "recipient"
        <*> o .?  "recipient_id"
        <*> o .?  "sender_id"
        <*> o .?? "coordinates"

instance encodeJsonDirectMessage :: EncodeJson DirectMessage where
  encodeJson :: DirectMessage -> Json
  encodeJson (DirectMessage directMessage) =
    "created_at" := HackyWrapperShouldBeTwitterTime directMessage.dmCreatedAt ~>
    "sender_screen_name" := directMessage.dmSenderScreenName ~>
    "sender" := directMessage.dmSender ~>
    "text" := directMessage.dmText ~>
    "recipient_screen_name" := directMessage.dmRecipientScreeName ~>
    "id" := directMessage.dmId ~>
    "recipient" := directMessage.dmRecipient ~>
    "recipient_id" := directMessage.dmRecipientId ~>
    "sender_id" := directMessage.dmSenderId ~>
    "coordinates" := directMessage.dmCoordinates ~>
    jsonEmptyObject

data EventType
  = Favorite
  | Unfavorite
  | ListCreated
  | ListUpdated
  | ListMemberAdded
  | UserUpdate
  | Block
  | Unblock
  | Follow

derive instance genericEventType :: Generic EventType _
derive instance eqEventType :: Eq EventType
instance showEventType :: Show EventType where show = genericShow


data EventTarget
  = ETUser User
  | ETStatus Status
  | ETList List'
  | ETUnknown Json

derive instance genericEventTarget :: Generic EventTarget _
derive instance eqEventTarget :: Eq EventTarget
instance showEventTarget :: Show EventTarget where show = genericShow

instance decodeJsonEventTarget :: DecodeJson EventTarget where
  decodeJson :: Json -> Either String EventTarget
  decodeJson =
    foldJsonObject (Left "not a JObject (EventTarget)") $ checkError' \o ->
      let value = fromObject o
      in
        ETUser <$> decodeJson value <|>
        ETStatus <$> decodeJson value <|>
        ETList <$> decodeJson value <|>
        pure (ETUnknown value)

instance encodeJsonEventTarget :: EncodeJson EventTarget where
  encodeJson :: EventTarget -> Json
  encodeJson (ETUser user) = encodeJson user
  encodeJson (ETStatus status) = encodeJson status
  encodeJson (ETList list) = encodeJson list
  encodeJson (ETUnknown unknown) = unknown

newtype Event = Event
  { evCreatedAt :: HackyDateTime
  , evTargetObject :: Maybe EventTarget
  , evEvent :: String
  , evTarget :: EventTarget
  , evSource :: EventTarget
  }

toEvent
  :: HackyDateTime
  -> Maybe EventTarget
  -> String
  -> EventTarget
  -> EventTarget
  -> Event
toEvent evCreatedAt evTargetObject evEvent evTarget evSource =
  Event
    { evCreatedAt
    , evTargetObject
    , evEvent
    , evTarget
    , evSource
    }

derive instance genericEvent :: Generic Event _
derive instance newtypeEvent :: Newtype Event _
derive instance eqEvent :: Eq Event
instance showEvent :: Show Event where show = genericShow

instance decodeJsonEvent :: DecodeJson Event where
  decodeJson :: Json -> Either String Event
  decodeJson =
    foldJsonObject (Left "not a JObject (Event)") $ checkError' \o ->
      toEvent
        <$> map fromHackyWrapperShouldBeTwitterTime (o .?  "created_at")
        <*> o .?? "target_object"
        <*> o .?  "event"
        <*> o .?  "target"
        <*> o .?  "source"

instance encodeJsonEvent :: EncodeJson Event where
  encodeJson :: Event -> Json
  encodeJson (Event event) =
    "created_at" := HackyWrapperShouldBeTwitterTime event.evCreatedAt ~>
    "target_object" := event.evTargetObject ~>
    "event"         := event.evEvent ~>
    "target"        := event.evTarget ~>
    "source"        := event.evSource ~>
    jsonEmptyObject

newtype Delete = Delete
  { delId :: StatusId
  , delUserId :: UserId
  }

toDelete :: StatusId -> UserId -> Delete
toDelete delId delUserId =
  Delete {delId, delUserId}

derive instance genericDelete :: Generic Delete _
derive instance newtypeDelete :: Newtype Delete _
derive instance eqDelete :: Eq Delete
instance showDelete :: Show Delete where show = genericShow

instance decodeJsonDelete :: DecodeJson Delete where
  decodeJson :: Json -> Either String Delete
  decodeJson =
    foldJsonObject (Left "not a JObject (Delete)") \o -> do
      deleteObj <- o .? "delete"
      statusObj <- deleteObj .? "status"
      toDelete
        <$> statusObj .? "id"
        <*> statusObj .? "user_id"

instance encodeJsonDelete :: EncodeJson Delete where
  encodeJson :: Delete -> Json
  encodeJson (Delete delete) =
    let statusObj =
          "id" := delete.delId ~>
          "user_id" := delete.delUserId ~>
          jsonEmptyObject
        deleteObj = "status" := statusObj ~> jsonEmptyObject
    in "delete" := deleteObj ~> jsonEmptyObject

---- | This type represents the Twitter user.
---- See <https://dev.twitter.com/docs/platform-objects/users>.
newtype User = User
  { userContributorsEnabled :: Boolean
  , userCreatedAt :: HackyDateTime
  , userDefaultProfile :: Boolean
  , userDefaultProfileImage :: Boolean
  , userDescription :: Maybe String
  , userEmail :: Maybe String
  , userFavoritesCount :: Int
  , userFollowRequestSent :: Maybe Boolean
  , userFollowing :: Maybe Boolean
  , userFollowersCount :: Int
  , userFriendsCount :: Int
  , userGeoEnabled :: Boolean
  , userId :: UserId
  , userIsTranslator :: Boolean
  , userLang :: LanguageCode
  , userListedCount :: Int
  , userLocation :: Maybe String
  , userName :: String
  , userNotifications :: Maybe Boolean
  , userProfileBackgroundColor :: Maybe String
  , userProfileBackgroundImageURL :: Maybe URIString
  , userProfileBackgroundImageURLHttps :: Maybe URIString
  , userProfileBackgroundTile :: Maybe Boolean
  , userProfileBannerURL :: Maybe URIString
  , userProfileImageURL :: Maybe URIString
  , userProfileImageURLHttps :: Maybe URIString
  , userProfileLinkColor :: String
  , userProfileSidebarBorderColor :: String
  , userProfileSidebarFillColor :: String
  , userProfileTextColor :: String
  , userProfileUseBackgroundImage :: Boolean
  , userProtected :: Boolean
  , userScreenName :: String
  , userShowAllInlineMedia :: Maybe Boolean
  , userStatusesCount :: Int
  , userTimeZone :: Maybe String
  , userURL :: Maybe URIString
  , userUtcOffset :: Maybe Int
  , userVerified :: Boolean
  , userWithheldInCountries :: Maybe (Array String)
  , userWithheldScope :: Maybe String
  }


toUser
  :: Boolean
  -> HackyDateTime
  -> Boolean
  -> Boolean
  -> Maybe String
  -> Maybe String
  -> Int
  -> Maybe Boolean
  -> Maybe Boolean
  -> Int
  -> Int
  -> Boolean
  -> UserId
  -> Boolean
  -> LanguageCode
  -> Int
  -> Maybe String
  -> String
  -> Maybe Boolean
  -> Maybe String
  -> Maybe URIString
  -> Maybe URIString
  -> Maybe Boolean
  -> Maybe URIString
  -> Maybe URIString
  -> Maybe URIString
  -> String
  -> String
  -> String
  -> String
  -> Boolean
  -> Boolean
  -> String
  -> Maybe Boolean
  -> Int
  -> Maybe String
  -> Maybe URIString
  -> Maybe Int
  -> Boolean
  -> Maybe (Array String)
  -> Maybe String
  -> User
toUser userContributorsEnabled userCreatedAt userDefaultProfile userDefaultProfileImage userDescription userEmail userFavoritesCount userFollowRequestSent userFollowing userFollowersCount userFriendsCount userGeoEnabled userId userIsTranslator userLang userListedCount userLocation userName userNotifications userProfileBackgroundColor userProfileBackgroundImageURL userProfileBackgroundImageURLHttps userProfileBackgroundTile userProfileBannerURL userProfileImageURL userProfileImageURLHttps userProfileLinkColor userProfileSidebarBorderColor userProfileSidebarFillColor userProfileTextColor userProfileUseBackgroundImage userProtected userScreenName userShowAllInlineMedia userStatusesCount userTimeZone userURL userUtcOffset userVerified userWithheldInCountries userWithheldScope =
  User
    { userContributorsEnabled
    , userCreatedAt
    , userDefaultProfile
    , userDefaultProfileImage
    , userDescription
    , userEmail
    , userFavoritesCount
    , userFollowRequestSent
    , userFollowing
    , userFollowersCount
    , userFriendsCount
    , userGeoEnabled
    , userId
    , userIsTranslator
    , userLang
    , userListedCount
    , userLocation
    , userName
    , userNotifications
    , userProfileBackgroundColor
    , userProfileBackgroundImageURL
    , userProfileBackgroundImageURLHttps
    , userProfileBackgroundTile
    , userProfileBannerURL
    , userProfileImageURL
    , userProfileImageURLHttps
    , userProfileLinkColor
    , userProfileSidebarBorderColor
    , userProfileSidebarFillColor
    , userProfileTextColor
    , userProfileUseBackgroundImage
    , userProtected
    , userScreenName
    , userShowAllInlineMedia
    , userStatusesCount
    , userTimeZone
    , userURL
    , userUtcOffset
    , userVerified
    , userWithheldInCountries
    , userWithheldScope
    }

derive instance genericUser :: Generic User _
derive instance newtypeUser :: Newtype User _
derive instance eqUser :: Eq User
instance showUser :: Show User where show = genericShow

instance decodeJsonUser :: DecodeJson User where
  decodeJson :: Json -> Either String User
  decodeJson =
    foldJsonObject (Left "not a JObject (User)") $ checkError' \o ->
      toUser
        <$> o .?  "contributors_enabled"
        <*> map fromHackyWrapperShouldBeTwitterTime (o .?  "created_at")
        <*> o .?  "default_profile"
        <*> o .?  "default_profile_image"
        <*> o .?? "description"
        -- TODO: Why is this written like this??
        -- <*> fmap join (o .?? "email") -- The field can be a null value
        <*> o .?? "email"
        <*> o .?  "favourites_count"
        <*> o .?? "follow_request_sent" -- .!= Nothing
        <*> o .?? "following" -- .!= Nothing
        <*> o .?  "followers_count"
        <*> o .?  "friends_count"
        <*> o .?  "geo_enabled"
        <*> o .?  "id"
        <*> o .?  "is_translator"
        <*> o .?  "lang"
        <*> o .?  "listed_count"
        <*> o .?? "location"
        <*> o .?  "name"
        <*> o .?? "notifications" -- .!= Nothing
        <*> o .?? "profile_background_color"
        <*> o .?? "profile_background_image_url"
        <*> o .?? "profile_background_image_url_https"
        <*> o .?? "profile_background_tile"
        <*> o .?? "profile_banner_url"
        <*> o .?? "profile_image_url"
        <*> o .?? "profile_image_url_https"
        <*> o .?  "profile_link_color"
        <*> o .?  "profile_sidebar_border_color"
        <*> o .?  "profile_sidebar_fill_color"
        <*> o .?  "profile_text_color"
        <*> o .?  "profile_use_background_image"
        <*> o .?  "protected"
        <*> o .?  "screen_name"
        <*> o .?? "show_all_inline_media"
        <*> o .?  "statuses_count"
        <*> o .?? "time_zone"
        <*> o .?? "url" -- .!= Nothing
        <*> o .?? "utc_offset"
        <*> o .?  "verified"
        <*> o .?? "withheld_in_countries"
        <*> o .?? "withheld_scope"

instance encodeJsonUser :: EncodeJson User where
  encodeJson :: User -> Json
  encodeJson (User user) =
    "contributors_enabled" := user.userContributorsEnabled ~>
    "created_at" := HackyWrapperShouldBeTwitterTime user.userCreatedAt ~>
    "default_profile" := user.userDefaultProfile ~>
    "default_profile_image" := user.userDefaultProfileImage ~>
    "description" := user.userDescription ~>
    "email" := user.userEmail ~>
    "favourites_count" := user.userFavoritesCount ~>
    "follow_request_sent" := user.userFollowRequestSent ~>
    "following" := user.userFollowing ~>
    "followers_count" := user.userFollowersCount ~>
    "friends_count" := user.userFriendsCount ~>
    "geo_enabled" := user.userGeoEnabled ~>
    "id" := user.userId ~>
    "is_translator" := user.userIsTranslator ~>
    "lang" := user.userLang ~>
    "listed_count" := user.userListedCount ~>
    "location" := user.userLocation ~>
    "name" := user.userName ~>
    "notifications" := user.userNotifications ~>
    "profile_background_color" := user.userProfileBackgroundColor ~>
    "profile_background_image_url" := user.userProfileBackgroundImageURL ~>
    "profile_background_image_url_https" := user.userProfileBackgroundImageURLHttps ~>
    "profile_background_tile" := user.userProfileBackgroundTile ~>
    "profile_banner_url" := user.userProfileBannerURL ~>
    "profile_image_url" := user.userProfileImageURL ~>
    "profile_image_url_https" := user.userProfileImageURLHttps ~>
    "profile_link_color" := user.userProfileLinkColor ~>
    "profile_sidebar_border_color" := user.userProfileSidebarBorderColor ~>
    "profile_sidebar_fill_color" := user.userProfileSidebarFillColor ~>
    "profile_text_color" := user.userProfileTextColor ~>
    "profile_use_background_image" := user.userProfileUseBackgroundImage ~>
    "protected" := user.userProtected ~>
    "screen_name" := user.userScreenName ~>
    "show_all_inline_media" := user.userShowAllInlineMedia ~>
    "statuses_count" := user.userStatusesCount ~>
    "time_zone" := user.userTimeZone ~>
    "url" := user.userURL ~>
    "utc_offset" := user.userUtcOffset ~>
    "verified" := user.userVerified ~>
    "withheld_in_countries" := user.userWithheldInCountries ~>
    "withheld_scope" := user.userWithheldScope ~>
    jsonEmptyObject



newtype List' = List'
  { listId :: Int
  , listName :: String
  , listFullName :: String
  , listMemberCount :: Int
  , listSubscriberCount :: Int
  , listMode :: String
  , listUser :: User
  }

toList'
  :: Int
  -> String
  -> String
  -> Int
  -> Int
  -> String
  -> User
  -> List'
toList' listId listName listFullName listMemberCount listSubscriberCount listMode listUser =
  List'
    { listId
    , listName
    , listFullName
    , listMemberCount
    , listSubscriberCount
    , listMode
    , listUser
    }

derive instance genericList' :: Generic List' _
derive instance newtypeList' :: Newtype List' _
derive instance eqList' :: Eq List'
instance showList' :: Show List' where show = genericShow

instance decodeJsonList' :: DecodeJson List' where
  decodeJson :: Json -> Either String List'
  decodeJson =
    foldJsonObject (Left "not a JObject (List')") $ checkError' \o ->
      toList'
        <$> o .?  "id"
        <*> o .?  "name"
        <*> o .?  "full_name"
        <*> o .?  "member_count"
        <*> o .?  "subscriber_count"
        <*> o .?  "mode"
        <*> o .?  "user"

instance encodeJsonList' :: EncodeJson List' where
  encodeJson :: List' -> Json
  encodeJson (List' list) =
    "id" := list.listId ~>
    "name" := list.listName ~>
    "full_name" := list.listFullName ~>
    "member_count" := list.listMemberCount ~>
    "subscriber_count" := list.listSubscriberCount ~>
    "mode" := list.listMode ~>
    "user" := list.listUser ~>
    jsonEmptyObject

-- | Hashtag entity.
-- See <https://dev.twitter.com/docs/platform-objects/entities#obj-hashtags>.
newtype HashTagEntity = HashTagEntity
  { hashTagText :: String
  }

toHashTagEntity :: String -> HashTagEntity
toHashTagEntity hashTagText =
  HashTagEntity {hashTagText}

derive instance genericHashTagEntity :: Generic HashTagEntity _
derive instance newtypeHashTagEntity :: Newtype HashTagEntity _
derive instance eqHashTagEntity :: Eq HashTagEntity
instance showHashTagEntity :: Show HashTagEntity where show = genericShow

instance decodeJsonHashTagEntity :: DecodeJson HashTagEntity where
  decodeJson :: Json -> Either String HashTagEntity
  decodeJson =
    foldJsonObject (Left "not a JObject (HashTagEntity)") \o ->
      toHashTagEntity <$> o .? "text"

instance encodeJsonHashTagEntity :: EncodeJson HashTagEntity where
  encodeJson :: HashTagEntity -> Json
  encodeJson (HashTagEntity hashTagEntity) =
    "text" := hashTagEntity.hashTagText ~>
    jsonEmptyObject

-- | User mention entity.
-- See <https://dev.twitter.com/docs/platform-objects/entities#obj-usermention>.
newtype UserEntity = UserEntity
  { userEntityUserId :: UserId
  , userEntityUserName :: UserName
  , userEntityUserScreenName :: String
  }

toUserEntity :: UserId -> UserName -> String -> UserEntity
toUserEntity userEntityUserId userEntityUserName userEntityUserScreenName =
  UserEntity {userEntityUserId, userEntityUserName, userEntityUserScreenName}

derive instance genericUserEntity :: Generic UserEntity _
derive instance newtypeUserEntity :: Newtype UserEntity _
derive instance eqUserEntity :: Eq UserEntity
instance showUserEntity :: Show UserEntity where show = genericShow

instance decodeJsonUserEntity :: DecodeJson UserEntity where
  decodeJson :: Json -> Either String UserEntity
  decodeJson =
    foldJsonObject (Left "not a JObject (UserEntity)") \o ->
      toUserEntity
        <$> o .? "id"
        <*> o .? "name"
        <*> o .? "screen_name"

instance encodeJsonUserEntity :: EncodeJson UserEntity where
  encodeJson :: UserEntity -> Json
  encodeJson (UserEntity userEntity) =
    "id" := userEntity.userEntityUserId ~>
    "name" := userEntity.userEntityUserName ~>
    "screen_name" := userEntity.userEntityUserScreenName ~>
    jsonEmptyObject

-- | URL entity.
-- | See <https://dev.twitter.com/docs/platform-objects/entities#obj-url>.
newtype URLEntity = URLEntity
  { ueURL :: URIString
    -- ^ The URL that was extracted
  , ueExpanded :: URIString
    -- ^ The fully resolved URL (only for t.co links)
  , ueDisplay :: String
    -- ^ Not a URL but a string to display instead of the URL (only for t.co links)
   }

toURLEntity :: URIString -> URIString -> String -> URLEntity
toURLEntity ueURL ueExpanded ueDisplay =
  URLEntity { ueURL, ueExpanded, ueDisplay }

derive instance genericURLEntity :: Generic URLEntity _
derive instance newtypeURLEntity :: Newtype URLEntity _
derive instance eqURLEntity :: Eq URLEntity
instance showURLEntity :: Show URLEntity where show = genericShow

instance decodeJsonURLEntity :: DecodeJson URLEntity where
  decodeJson :: Json -> Either String URLEntity
  decodeJson =
    foldJsonObject (Left "not a JObject (URLEntity)") \o ->
      toURLEntity
        <$> o .? "url"
        <*> o .? "expanded_url"
        <*> o .? "display_url"

instance encodeJsonURLEntity :: EncodeJson URLEntity where
  encodeJson :: URLEntity -> Json
  encodeJson (URLEntity urlEntity) =
    "url" := urlEntity.ueURL ~>
    "expanded_url" := urlEntity.ueExpanded ~>
    "display_url" := urlEntity.ueDisplay ~>
    jsonEmptyObject


newtype MediaEntity = MediaEntity
  { meType :: String
  , meId :: StatusId
  , meSizes :: StrMap MediaSize
  , meMediaURL :: URIString
  , meMediaURLHttps :: URIString
  , meURL :: URLEntity
  }

toMediaEntity
  :: String
  -> StatusId
  -> StrMap MediaSize
  -> URIString
  -> URIString
  -> URLEntity
  -> MediaEntity
toMediaEntity meType meId meSizes meMediaURL meMediaURLHttps meURL =
  MediaEntity {meType, meId, meSizes, meMediaURL, meMediaURLHttps, meURL}

derive instance genericMediaEntity :: Generic MediaEntity _
derive instance newtypeMediaEntity :: Newtype MediaEntity _
derive instance eqMediaEntity :: Eq MediaEntity
instance showMediaEntity :: Show MediaEntity where show = genericShow

instance decodeJsonMediaEntity :: DecodeJson MediaEntity where
  decodeJson :: Json -> Either String MediaEntity
  decodeJson json =
    foldJsonObject (Left "not a JObject (MediaEntity)") f json
    where
      f :: JObject -> Either String MediaEntity
      f o =
        toMediaEntity
          <$> o .? "type"
          <*> o .? "id"
          <*> o .? "sizes"
          <*> o .? "media_url"
          <*> o .? "media_url_https"
          <*> decodeJson json

instance encodeJsonMediaEntity :: EncodeJson MediaEntity where
  encodeJson :: MediaEntity -> Json
  encodeJson (MediaEntity mediaEntity) =
    "type" := mediaEntity.meType ~>
    "id" := mediaEntity.meId ~>
    "sizes" := mediaEntity.meSizes ~>
    "media_url" := mediaEntity.meMediaURL ~>
    "media_url_https" := mediaEntity.meMediaURLHttps ~>
    "url" := _.ueURL (un URLEntity mediaEntity.meURL) ~>
    "expanded_url" := _.ueExpanded (un URLEntity mediaEntity.meURL) ~>
    "dislay_url" := _.ueDisplay (un URLEntity mediaEntity.meURL) ~>
    jsonEmptyObject

-- | Size entity.
-- See <https://dev.twitter.com/docs/platform-objects/entities#obj-size>.
newtype MediaSize = MediaSize
  { msWidth :: Int
  , msHeight :: Int
  , msResize :: String
  }

toMediaSize :: Int -> Int -> String -> MediaSize
toMediaSize msWidth msHeight msResize =
  MediaSize { msWidth, msHeight, msResize }

derive instance genericMediaSize :: Generic MediaSize _
derive instance newtypeMediaSize :: Newtype MediaSize _
derive instance eqMediaSize :: Eq MediaSize
instance showMediaSize :: Show MediaSize where show = genericShow

instance decodeJsonMediaSize :: DecodeJson MediaSize where
  decodeJson :: Json -> Either String MediaSize
  decodeJson =
    foldJsonObject (Left "not a JObject (MediaSize)") \o ->
      toMediaSize
        <$> o .? "w"
        <*> o .? "h"
        <*> o .? "resize"

instance encodeJsonMediaSize :: EncodeJson MediaSize where
  encodeJson :: MediaSize -> Json
  encodeJson (MediaSize mediaSize) =
    "w" := mediaSize.msWidth ~>
    "h" := mediaSize.msHeight ~>
    "resize" := mediaSize.msResize ~>
    jsonEmptyObject

newtype Coordinates = Coordinates
  { coordinates :: Array Number
  , coordinatesType :: String
  }

toCoordinates :: Array Number -> String -> Coordinates
toCoordinates coord t =
  Coordinates
    { coordinates: coord
    , coordinatesType: t
    }

derive instance genericCoordinates :: Generic Coordinates _
derive instance newtypeCoordinates :: Newtype Coordinates _
derive instance eqCoordinates :: Eq Coordinates
instance showCoordinates :: Show Coordinates where show = genericShow

instance decodeJsonCoordinates :: DecodeJson Coordinates where
  decodeJson :: Json -> Either String Coordinates
  decodeJson =
    foldJsonObject (Left "not a JObject (Coordinates)") \o ->
      toCoordinates
        <$> o .? "coordinates"
        <*> o .? "type"

instance encodeJsonCoordinates :: EncodeJson Coordinates where
  encodeJson :: Coordinates -> Json
  encodeJson (Coordinates coordinates) =
    "coordinates" := coordinates.coordinates ~>
    "type" := coordinates.coordinatesType ~>
    jsonEmptyObject

-- | This type represents a place, named locations with corresponding geo coordinates.
-- | See <https://dev.twitter.com/docs/platform-objects/places>.
newtype Place = Place
  { placeAttributes :: StrMap String
  , placeBoundingBox :: Maybe BoundingBox
  , placeCountry :: String
  , placeCountryCode :: String
  , placeFullName :: String
  , placeId :: String
  , placeName :: String
  , placeType :: String
  , placeURL :: String
  }

toPlace
  :: StrMap String
  -> Maybe BoundingBox
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> Place
toPlace attr bb country cc fullname plid name t url =
  Place
    { placeAttributes: attr
    , placeBoundingBox: bb
    , placeCountry: country
    , placeCountryCode: cc
    , placeFullName: fullname
    , placeId: plid
    , placeName: name
    , placeType: t
    , placeURL: url
    }

derive instance genericPlace :: Generic Place _
derive instance newtypePlace :: Newtype Place _
derive instance eqPlace :: Eq Place
instance showPlace :: Show Place where show = genericShow

instance decodeJsonPlace :: DecodeJson Place where
  decodeJson :: Json -> Either String Place
  decodeJson =
    foldJsonObject (Left "not a JObject (Place)") \o ->
      toPlace
        <$> o .? "attributes"
        <*> o .?? "bounding_box"
        <*> o .? "country"
        <*> o .? "country_code"
        <*> o .? "full_name"
        <*> o .? "id"
        <*> o .? "name"
        <*> o .? "place_type"
        <*> o .? "url"

instance encodeJsonPlace :: EncodeJson Place where
  encodeJson :: Place -> Json
  encodeJson (Place place) =
    "attributes" := place.placeAttributes ~>
    "bounding_box" := place.placeBoundingBox ~>
    "country" := place.placeCountry ~>
    "country_code" := place.placeCountryCode ~>
    "full_name" := place.placeFullName ~>
    "id" := place.placeId ~>
    "name" := place.placeName ~>
    "place_type" := place.placeType ~>
    "url" := place.placeURL ~>
    jsonEmptyObject

-- | A bounding box of coordinates which encloses the place.
-- See <https://dev.twitter.com/docs/platform-objects/places#obj-boundingbox>.
newtype BoundingBox = BoundingBox
  { boundingBoxCoordinates :: Array (Array (Array Number))
  , boundingBoxType :: String
  }

toBoundingBox :: Array (Array (Array Number)) -> String -> BoundingBox
toBoundingBox boundingBoxCoordinates boundingBoxType =
  BoundingBox
    { boundingBoxCoordinates
    , boundingBoxType
    }

derive instance genericBoundingBox :: Generic BoundingBox _
derive instance newtypeBoundingBox :: Newtype BoundingBox _
derive instance eqBoundingBox :: Eq BoundingBox
instance showBoundingBox :: Show BoundingBox where show = genericShow

instance decodeJsonBoundingBox :: DecodeJson BoundingBox where
  decodeJson :: Json -> Either String BoundingBox
  decodeJson =
    foldJsonObject (Left "not a JObject (BoundingBox)") \o ->
      toBoundingBox
        <$> o .? "coordinates"
        <*> o .? "type"

instance encodeJsonBoundingBox :: EncodeJson BoundingBox where
  encodeJson :: BoundingBox -> Json
  encodeJson (BoundingBox boundingBox) =
    "coordinates" := boundingBox.boundingBoxCoordinates ~>
    "type" := boundingBox.boundingBoxType ~>
    jsonEmptyObject

-- | Entity handling.
-- | See <https://dev.twitter.com/docs/platform-objects/entities>.
newtype Entities = Entities
  { enHashTags :: Array (Entity HashTagEntity)
  , enUserMentions :: Array (Entity UserEntity)
  , enURLs :: Array (Entity URLEntity)
  , enMedia :: Array (Entity MediaEntity)
  }

toEntities
  :: Array (Entity HashTagEntity)
  -> Array (Entity UserEntity)
  -> Array (Entity URLEntity)
  -> Array (Entity MediaEntity)
  -> Entities
toEntities enHashTags enUserMentions enURLs enMedia =
  Entities {enHashTags, enUserMentions, enURLs, enMedia}

derive instance genericEntities :: Generic Entities _
derive instance newtypeEntities :: Newtype Entities _
derive instance eqEntities :: Eq Entities
instance showEntities :: Show Entities where show = genericShow

instance decodeJsonEntities :: DecodeJson Entities where
  decodeJson :: Json -> Either String Entities
  decodeJson =
    foldJsonObject (Left "not a JObject (Entities)") \o ->
      toEntities
       <$> o .?? "hashtags" .?= []
       <*> o .?? "user_mentions" .?= []
       <*> o .?? "urls" .?= []
       <*> o .?? "media" .?= []

instance encodeJsonEntities :: EncodeJson Entities where
  encodeJson :: Entities -> Json
  encodeJson (Entities entities) =
    "hashtags" := entities.enHashTags ~>
    "user_mentions" := entities.enUserMentions ~>
    "urls" := entities.enURLs ~>
    "media" := entities.enMedia ~>
    jsonEmptyObject

-- | The character positions the Entity was extracted from
-- |
-- | This is experimental implementation.
-- | This may be replaced by more definite types.
type EntityIndices = Array Int

newtype Entity a =
    Entity
    { entityBody :: a
    -- ^ The detail information of specific entity types (HashTag, URL, User)
    , entityIndices :: EntityIndices
    -- ^ The character positions the Entity was extracted from
    }

toEntity :: forall a. a -> EntityIndices -> Entity a
toEntity entityBody entityIndices =
  Entity {entityBody, entityIndices}

derive instance genericEntity :: Generic (Entity a) _
derive instance newtypeEntity :: Newtype (Entity a) _
derive instance eqEntity :: Eq a => Eq (Entity a)
instance showEntity :: Show a => Show (Entity a) where show = genericShow

instance decodeJsonEntity :: DecodeJson a => DecodeJson (Entity a) where
  decodeJson :: Json -> Either String (Entity a)
  decodeJson =
    foldJsonObject (Left "not a JObject (Entity)") \o ->
      toEntity
        <$> decodeJson (fromObject o)
        <*> o .? "indices"

instance encodeJsonEntity :: EncodeJson a => EncodeJson (Entity a) where
  encodeJson :: Entity a -> Json
  encodeJson (Entity entity) =
    let json = encodeJson entity.entityBody
        maybeJObject = toObject json
    in case maybeJObject of
        Nothing -> json
        Just obj ->
          fromObject $ insert "indices" (encodeJson entity.entityIndices) obj

newtype Contributor = Contributor
    { contributorId :: UserId
    , contributorScreenName :: Maybe String
    }

toContributor :: Int -> Maybe String -> Contributor
toContributor contribId screenName =
  Contributor
    { contributorId: contribId
    , contributorScreenName: screenName
    }

derive instance genericContributor :: Generic Contributor _
derive instance newtypeContributor :: Newtype Contributor _
derive instance eqContributor :: Eq Contributor
instance showContributor :: Show Contributor where show = genericShow

instance decodeJsonContributor :: DecodeJson Contributor where
  decodeJson :: Json -> Either String Contributor
  decodeJson =
    foldJson
      (const err)
      (const err)
      num
      (const err)
      (const err)
      obj
    where
      err :: forall a. Either String a
      err = Left "not a JObject or JNumber (Contributor)"

      obj :: JObject -> Either String Contributor
      obj o =
        toContributor
          <$> o .? "id"
          <*> o .?? "screen_name"

      num :: JNumber -> Either String Contributor
      num jnum =
        case fromNumber jnum of
          Nothing -> Left "not an Int (Contributor)"
          Just n -> Right $ toContributor n Nothing

instance encodeJsonContributor :: EncodeJson Contributor where
  encodeJson :: Contributor -> Json
  encodeJson (Contributor contributor) =
    "id" := contributor.contributorId ~>
    "screen_name" := contributor.contributorScreenName ~>
    jsonEmptyObject

-- | Image size type. This type is included in the API response of
-- \"\/1.1\/media\/upload.json\".
newtype ImageSizeType = ImageSizeType
    { imageSizeTypeWidth :: Int
    , imageSizeTypeHeight :: Int
    , imageSizeTypeType :: String
    }

toImageSizeType :: Int -> Int -> String -> ImageSizeType
toImageSizeType w h t =
  ImageSizeType
    { imageSizeTypeWidth: w
    , imageSizeTypeHeight: h
    , imageSizeTypeType: t
    }

derive instance genericImageSizeType :: Generic ImageSizeType _
derive instance newtypeImageSizeType :: Newtype ImageSizeType _
derive instance eqImageSizeType :: Eq ImageSizeType
instance showImageSizeType :: Show ImageSizeType where show = genericShow

instance decodeJsonImageSizeType :: DecodeJson ImageSizeType where
  decodeJson :: Json -> Either String ImageSizeType
  decodeJson =
    foldJsonObject (Left "not a JObject (ImageSizeType)") \o ->
      toImageSizeType
        <$> o .? "w"
        <*> o .? "h"
        <*> o .? "image_type"

instance encodeJsonImageSizeType :: EncodeJson ImageSizeType where
  encodeJson :: ImageSizeType -> Json
  encodeJson (ImageSizeType imageSizeType) =
    "w" := imageSizeType.imageSizeTypeWidth ~>
    "h" := imageSizeType.imageSizeTypeHeight ~>
    "image_type" := imageSizeType.imageSizeTypeType ~>
    jsonEmptyObject

-- | This type is represents the API response of \"\/1.1\/media\/upload.json\".
-- See <https://dev.twitter.com/docs/api/multiple-media-extended-entities>.
newtype UploadedMedia = UploadedMedia
    { uploadedMediaId :: Int
    , uploadedMediaSize :: Int
    , uploadedMediaImage :: ImageSizeType
    }

toUploadedMedia :: Int -> Int -> ImageSizeType -> UploadedMedia
toUploadedMedia mediaId size image =
  UploadedMedia
    { uploadedMediaId: mediaId
    , uploadedMediaSize: size
    , uploadedMediaImage: image
    }

derive instance genericUploadedMedia :: Generic UploadedMedia _
derive instance newtypeUploadedMedia :: Newtype UploadedMedia _
derive instance eqUploadedMedia :: Eq UploadedMedia
instance showUploadedMedia :: Show UploadedMedia where show = genericShow

instance decodeJsonUploadedMedia :: DecodeJson UploadedMedia where
  decodeJson :: Json -> Either String UploadedMedia
  decodeJson =
    foldJsonObject (Left "not a JObject (UploadedMedia)") \o ->
      toUploadedMedia
        <$> o .? "media_id"
        <*> o .? "size"
        <*> o .? "image"

instance encodeJsonUploadedMedia :: EncodeJson UploadedMedia where
  encodeJson :: UploadedMedia -> Json
  encodeJson (UploadedMedia uploadedMedia) =
    "media_id" := uploadedMedia.uploadedMediaId ~>
    "size" := uploadedMedia.uploadedMediaSize ~>
    "image" := uploadedMedia.uploadedMediaImage ~>
    jsonEmptyObject
