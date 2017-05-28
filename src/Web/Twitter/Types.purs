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
       -- , List(..)
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

import Data.Argonaut
  (class DecodeJson, class EncodeJson, JNumber, JObject, (.?), (:=), (~>),
   decodeJson, foldJson, foldJsonObject, jsonEmptyObject)
import Data.Argonaut.Decode.Combinators ((.??))
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.StrMap (StrMap)

--import Control.Applicative
--import Control.Monad
--import Data.Aeson
--import Data.Aeson.Types (Parser)
--import Data.Data
--import Data.HashMap.Strict (HashMap, fromList, union)
--import Data.Text (Text, unpack, pack)
--import GHC.Generics
--#if MIN_VERSION_time(1, 5, 0)
--import Data.Time
--#else
--import Data.Time
--import System.Locale
--#endif

newtype TwitterTime = TwitterTime DateTime
--newtype TwitterTime = TwitterTime { fromTwitterTime :: UTCTime }

fromTwitterTime :: TwitterTime -> DateTime
fromTwitterTime (TwitterTime dateTime) = dateTime


type UserId       = Int
type Friends      = List UserId
type URIString    = String
type UserName     = String
type StatusId     = Int
type LanguageCode = String

--data StreamingAPI = SStatus Status
--                  | SRetweetedStatus RetweetedStatus
--                  | SEvent Event
--                  | SDelete Delete
--                  -- | SScrubGeo ScrubGeo
--                  | SFriends Friends
--                  | SDirectMessage DirectMessage
--                  | SUnknown Value
--                  deriving (Show, Eq, Data, Typeable, Generic)


checkError :: JObject -> Either String Unit
checkError o = do
  err <- o .? "error"
  go err
  where
    go :: Either String String -> Either String Unit
    go (Right msg) = Left msg
    go (Left _) = Right unit

checkError'
  :: forall a. (JObject -> Either String a) -> JObject -> Either String a
checkError' realFunc o =
  checkError o *> realFunc o

twitterTimeFormat :: String
twitterTimeFormat = "%a %b %d %T %z %Y"
-- Sat Jun 14 10:15:19 +0000 2014

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

--instance FromJSON StreamingAPI where
--    parseJSON v@(Object o) =
--        SRetweetedStatus <$> js <|>
--        SStatus <$> js <|>
--        SEvent <$> js <|>
--        SDelete <$> js <|>
--        SFriends <$> (o .: "friends") <|>
--        SDirectMessage <$> (o .: "direct_message") <|>
--        return (SUnknown v)
--      where
--        js :: FromJSON a => Parser a
--        js = parseJSON v
--    parseJSON v = fail $ "couldn't parse StreamingAPI from: " ++ show v

--instance ToJSON StreamingAPI where
--    toJSON (SStatus          s) = toJSON s
--    toJSON (SRetweetedStatus s) = toJSON s
--    toJSON (SEvent           e) = toJSON e
--    toJSON (SDelete          d) = toJSON d
--    toJSON (SFriends         f) = toJSON f
--    toJSON (SDirectMessage   m) = toJSON m
--    toJSON (SUnknown         v) = v

---- | This type represents a Twitter tweet structure.
---- See <https://dev.twitter.com/docs/platform-objects/tweets>.
--data Status = Status
--    { statusContributors :: Maybe [Contributor]
--    , statusCoordinates :: Maybe Coordinates
--    , statusCreatedAt :: UTCTime
--    , statusCurrentUserRetweet :: Maybe StatusId
--    , statusEntities :: Maybe Entities
--    , statusExtendedEntities :: Maybe Entities
--    , statusFavoriteCount :: Integer
--    , statusFavorited :: Maybe Boolean
--    , statusFilterLevel :: Maybe Text
--    , statusId :: StatusId
--    , statusInReplyToScreenName :: Maybe Text
--    , statusInReplyToStatusId :: Maybe StatusId
--    , statusInReplyToUserId :: Maybe UserId
--    , statusLang :: Maybe LanguageCode
--    , statusPlace :: Maybe Place
--    , statusPossiblySensitive :: Maybe Boolean
--    , statusScopes :: Maybe Object
--    , statusQuotedStatusId :: Maybe StatusId
--    , statusQuotedStatus :: Maybe Status
--    , statusRetweetCount :: Integer
--    , statusRetweeted :: Maybe Boolean
--    , statusRetweetedStatus :: Maybe Status
--    , statusSource :: Text
--    , statusText :: Text
--    , statusTruncated :: Boolean
--    , statusUser :: User
--    , statusWithheldCopyright :: Maybe Boolean
--    , statusWithheldInCountries :: Maybe [Text]
--    , statusWithheldScope :: Maybe Text
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON Status where
--    parseJSON (Object o) = checkError o >>
--        Status <$> o .:? "contributors" .!= Nothing
--               <*> o .:? "coordinates" .!= Nothing
--               <*> (o .:  "created_at" >>= return . fromTwitterTime)
--               <*> ((o .: "current_user_retweet" >>= (.: "id")) <|> return Nothing)
--               <*> o .:? "entities"
--               <*> o .:? "extended_entities"
--               <*> o .:? "favorite_count" .!= 0
--               <*> o .:? "favorited"
--               <*> o .:? "filter_level"
--               <*> o .:  "id"
--               <*> o .:? "in_reply_to_screen_name" .!= Nothing
--               <*> o .:? "in_reply_to_status_id" .!= Nothing
--               <*> o .:? "in_reply_to_user_id" .!= Nothing
--               <*> o .:? "lang"
--               <*> o .:? "place" .!= Nothing
--               <*> o .:? "possibly_sensitive"
--               <*> o .:? "scopes"
--               <*> o .:? "quoted_status_id"
--               <*> o .:? "quoted_status"
--               <*> o .:? "retweet_count" .!= 0
--               <*> o .:? "retweeted"
--               <*> o .:? "retweeted_status"
--               <*> o .:  "source"
--               <*> o .:  "text"
--               <*> o .:  "truncated"
--               <*> o .:  "user"
--               <*> o .:? "withheld_copyright"
--               <*> o .:? "withheld_in_countries"
--               <*> o .:? "withheld_scope"
--    parseJSON v = fail $ "couldn't parse status from: " ++ show v

--instance ToJSON Status where
--    toJSON Status{..} = object [ "contributors"             .= statusContributors
--                               , "coordinates"              .= statusCoordinates
--                               , "created_at"               .= TwitterTime statusCreatedAt
--                               , "current_user_retweet"     .= object [ "id"     .= statusCurrentUserRetweet
--                                                                      , "id_str" .= show statusCurrentUserRetweet
--                                                                      ]
--                               , "entities"                 .= statusEntities
--                               , "extended_entities"        .= statusExtendedEntities
--                               , "favorite_count"           .= statusFavoriteCount
--                               , "favorited"                .= statusFavorited
--                               , "filter_level"             .= statusFilterLevel
--                               , "id"                       .= statusId
--                               , "in_reply_to_screen_name"  .= statusInReplyToScreenName
--                               , "in_reply_to_status_id"    .= statusInReplyToStatusId
--                               , "in_reply_to_user_id"      .= statusInReplyToUserId
--                               , "lang"                     .= statusLang
--                               , "place"                    .= statusPlace
--                               , "possibly_sensitive"       .= statusPossiblySensitive
--                               , "scopes"                   .= statusScopes
--                               , "quoted_status_id"         .= statusQuotedStatusId
--                               , "quoted_status"            .= statusQuotedStatus
--                               , "retweet_count"            .= statusRetweetCount
--                               , "retweeted"                .= statusRetweeted
--                               , "retweeted_status"         .= statusRetweetedStatus
--                               , "source"                   .= statusSource
--                               , "text"                     .= statusText
--                               , "truncated"                .= statusTruncated
--                               , "user"                     .= statusUser
--                               , "withheld_copyright"       .= statusWithheldCopyright
--                               , "withheld_in_countries"    .= statusWithheldInCountries
--                               , "withheld_scope"           .= statusWithheldScope
--                               ]

--data SearchResult body =
--    SearchResult
--    { searchResultStatuses :: body
--    , searchResultSearchMetadata :: SearchMetadata
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON body =>
--         FromJSON (SearchResult body) where
--    parseJSON (Object o) = checkError o >>
--        SearchResult <$> o .:  "statuses"
--                     <*> o .:  "search_metadata"
--    parseJSON v = fail $ "couldn't parse search result from: " ++ show v

--instance ToJSON body =>
--         ToJSON (SearchResult body) where
--    toJSON SearchResult{..} = object [ "statuses"        .= searchResultStatuses
--                                     , "search_metadata" .= searchResultSearchMetadata
--                                     ]

--data SearchStatus =
--    SearchStatus
--    { searchStatusCreatedAt     :: UTCTime
--    , searchStatusId            :: StatusId
--    , searchStatusText          :: Text
--    , searchStatusSource        :: Text
--    , searchStatusUser          :: User
--    , searchStatusCoordinates   :: Maybe Coordinates
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON SearchStatus where
--    parseJSON (Object o) = checkError o >>
--        SearchStatus <$> (o .:  "created_at" >>= return . fromTwitterTime)
--                     <*> o .:  "id"
--                     <*> o .:  "text"
--                     <*> o .:  "source"
--                     <*> o .:  "user"
--                     <*> o .:? "coordinates"
--    parseJSON v = fail $ "couldn't parse status search result from: " ++ show v

--instance ToJSON SearchStatus where
--    toJSON SearchStatus{..} = object [ "created_at"     .= TwitterTime searchStatusCreatedAt
--                                     , "id"             .= searchStatusId
--                                     , "text"           .= searchStatusText
--                                     , "source"         .= searchStatusSource
--                                     , "user"           .= searchStatusUser
--                                     , "coordinates"    .= searchStatusCoordinates
--                                     ]

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
instance eqSearchMetadata :: Eq SearchMetadata where eq = genericEq
instance showSearchMetadata :: Show SearchMetadata where show = genericShow

instance decodeJsonSearchMetadata :: DecodeJson SearchMetadata where
  -- decodeJson :: Json -> Either String SearchMetadata
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
  -- encodeJson :: SearchMetadata -> Json
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

--data RetweetedStatus =
--    RetweetedStatus
--    { rsCreatedAt       :: UTCTime
--    , rsId              :: StatusId
--    , rsText            :: Text
--    , rsSource          :: Text
--    , rsTruncated       :: Boolean
--    , rsEntities        :: Maybe Entities
--    , rsUser            :: User
--    , rsRetweetedStatus :: Status
--    , rsCoordinates     :: Maybe Coordinates
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON RetweetedStatus where
--    parseJSON (Object o) = checkError o >>
--        RetweetedStatus <$> (o .:  "created_at" >>= return . fromTwitterTime)
--                        <*> o .:  "id"
--                        <*> o .:  "text"
--                        <*> o .:  "source"
--                        <*> o .:  "truncated"
--                        <*> o .:? "entities"
--                        <*> o .:  "user"
--                        <*> o .:  "retweeted_status"
--                        <*> o .:? "coordinates"
--    parseJSON v = fail $ "couldn't parse retweeted status from: " ++ show v

--instance ToJSON RetweetedStatus where
--    toJSON RetweetedStatus{..} = object [ "created_at"          .= TwitterTime rsCreatedAt
--                                        , "id"                  .= rsId
--                                        , "text"                .= rsText
--                                        , "source"              .= rsSource
--                                        , "truncated"           .= rsTruncated
--                                        , "entities"            .= rsEntities
--                                        , "user"                .= rsUser
--                                        , "retweeted_status"    .= rsRetweetedStatus
--                                        , "coordinates"         .= rsCoordinates
--                                        ]

--data DirectMessage =
--    DirectMessage
--    { dmCreatedAt          :: UTCTime
--    , dmSenderScreenName   :: Text
--    , dmSender             :: User
--    , dmText               :: Text
--    , dmRecipientScreeName :: Text
--    , dmId                 :: StatusId
--    , dmRecipient          :: User
--    , dmRecipientId        :: UserId
--    , dmSenderId           :: UserId
--    , dmCoordinates        :: Maybe Coordinates
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON DirectMessage where
--    parseJSON (Object o) = checkError o >>
--        DirectMessage <$> (o .:  "created_at" >>= return . fromTwitterTime)
--                      <*> o .:  "sender_screen_name"
--                      <*> o .:  "sender"
--                      <*> o .:  "text"
--                      <*> o .:  "recipient_screen_name"
--                      <*> o .:  "id"
--                      <*> o .:  "recipient"
--                      <*> o .:  "recipient_id"
--                      <*> o .:  "sender_id"
--                      <*> o .:? "coordinates"
--    parseJSON v = fail $ "couldn't parse direct message from: " ++ show v

--instance ToJSON DirectMessage where
--    toJSON DirectMessage{..} = object [ "created_at"            .= TwitterTime dmCreatedAt
--                                      , "sender_screen_name"    .= dmSenderScreenName
--                                      , "sender"                .= dmSender
--                                      , "text"                  .= dmText
--                                      , "recipient_screen_name" .= dmRecipientScreeName
--                                      , "id"                    .= dmId
--                                      , "recipient"             .= dmRecipient
--                                      , "recipient_id"          .= dmRecipientId
--                                      , "sender_id"             .= dmSenderId
--                                      , "coordinates"           .= dmCoordinates
--                                      ]

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
instance eqEventType :: Eq EventType where eq = genericEq
instance showEventType :: Show EventType where show = genericShow


--data EventTarget = ETUser User | ETStatus Status | ETList List | ETUnknown Value
--                 deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON EventTarget where
--    parseJSON v@(Object o) = checkError o >>
--        ETUser <$> parseJSON v <|>
--        ETStatus <$> parseJSON v <|>
--        ETList <$> parseJSON v <|>
--        return (ETUnknown v)
--    parseJSON v = fail $ "couldn't parse event target from: " ++ show v

--instance ToJSON EventTarget where
--    toJSON (ETUser    u) = toJSON u
--    toJSON (ETStatus  s) = toJSON s
--    toJSON (ETList    l) = toJSON l
--    toJSON (ETUnknown v) = v

--data Event =
--    Event
--    { evCreatedAt       :: UTCTime
--    , evTargetObject    :: Maybe EventTarget
--    , evEvent           :: Text
--    , evTarget          :: EventTarget
--    , evSource          :: EventTarget
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON Event where
--    parseJSON (Object o) = checkError o >>
--        Event <$> (o .:  "created_at" >>= return . fromTwitterTime)
--              <*> o .:? "target_object"
--              <*> o .:  "event"
--              <*> o .:  "target"
--              <*> o .:  "source"
--    parseJSON v = fail $ "couldn't parse event from: " ++ show v

--instance ToJSON Event where
--    toJSON Event{..} = object [ "created_at"    .= TwitterTime evCreatedAt
--                              , "target_object" .= evTargetObject
--                              , "event"         .= evEvent
--                              , "target"        .= evTarget
--                              , "source"        .= evSource
--                              ]

newtype Delete = Delete
  { delId  :: StatusId
  , delUserId :: UserId
  }

toDelete :: StatusId -> UserId -> Delete
toDelete delId delUserId =
  Delete {delId, delUserId}

derive instance genericDelete :: Generic Delete _
derive instance newtypeDelete :: Newtype Delete _
instance eqDelete :: Eq Delete where eq = genericEq
instance showDelete :: Show Delete where show = genericShow

instance decodeJsonDelete :: DecodeJson Delete where
  -- decodeJson :: Json -> Either String Delete
  decodeJson =
    foldJsonObject (Left "not a JObject (Delete)") \o -> do
      deleteObj <- o .? "delete"
      statusObj <- deleteObj .? "status"
      toDelete
        <$> statusObj .? "id"
        <*> statusObj .? "user_id"

instance encodeJsonDelete :: EncodeJson Delete where
  -- encodeJson :: Delete -> Json
  encodeJson (Delete delete) =
    let statusObj =
          "id" := delete.delId ~>
          "user_id" := delete.delUserId ~>
          jsonEmptyObject
        deleteObj = "status" := statusObj ~> jsonEmptyObject
    in "delete" := deleteObj ~> jsonEmptyObject

---- | This type represents the Twitter user.
---- See <https://dev.twitter.com/docs/platform-objects/users>.
--data User = User
--    { userContributorsEnabled :: Boolean
--    , userCreatedAt :: UTCTime
--    , userDefaultProfile :: Boolean
--    , userDefaultProfileImage :: Boolean
--    , userDescription :: Maybe Text
--    , userEmail :: Maybe Text
--    , userFavoritesCount :: Int
--    , userFollowRequestSent :: Maybe Boolean
--    , userFollowing :: Maybe Boolean
--    , userFollowersCount :: Int
--    , userFriendsCount :: Int
--    , userGeoEnabled :: Boolean
--    , userId :: UserId
--    , userIsTranslator :: Boolean
--    , userLang :: LanguageCode
--    , userListedCount :: Int
--    , userLocation :: Maybe Text
--    , userName :: Text
--    , userNotifications :: Maybe Boolean
--    , userProfileBackgroundColor :: Maybe Text
--    , userProfileBackgroundImageURL :: Maybe URIString
--    , userProfileBackgroundImageURLHttps :: Maybe URIString
--    , userProfileBackgroundTile :: Maybe Boolean
--    , userProfileBannerURL :: Maybe URIString
--    , userProfileImageURL :: Maybe URIString
--    , userProfileImageURLHttps :: Maybe URIString
--    , userProfileLinkColor :: Text
--    , userProfileSidebarBorderColor :: Text
--    , userProfileSidebarFillColor :: Text
--    , userProfileTextColor :: Text
--    , userProfileUseBackgroundImage :: Boolean
--    , userProtected :: Boolean
--    , userScreenName :: Text
--    , userShowAllInlineMedia :: Maybe Boolean
--    , userStatusesCount :: Int
--    , userTimeZone :: Maybe Text
--    , userURL :: Maybe URIString
--    , userUtcOffset :: Maybe Int
--    , userVerified :: Boolean
--    , userWithheldInCountries :: Maybe [Text]
--    , userWithheldScope :: Maybe Text
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON User where
--    parseJSON (Object o) = checkError o >>
--        User <$> o .:  "contributors_enabled"
--             <*> (o .:  "created_at" >>= return . fromTwitterTime)
--             <*> o .:  "default_profile"
--             <*> o .:  "default_profile_image"
--             <*> o .:? "description"
--             <*> fmap join (o .:? "email") -- The field can be a null value
--             <*> o .:  "favourites_count"
--             <*> o .:? "follow_request_sent" .!= Nothing
--             <*> o .:? "following" .!= Nothing
--             <*> o .:  "followers_count"
--             <*> o .:  "friends_count"
--             <*> o .:  "geo_enabled"
--             <*> o .:  "id"
--             <*> o .:  "is_translator"
--             <*> o .:  "lang"
--             <*> o .:  "listed_count"
--             <*> o .:? "location"
--             <*> o .:  "name"
--             <*> o .:? "notifications" .!= Nothing
--             <*> o .:? "profile_background_color"
--             <*> o .:? "profile_background_image_url"
--             <*> o .:? "profile_background_image_url_https"
--             <*> o .:? "profile_background_tile"
--             <*> o .:? "profile_banner_url"
--             <*> o .:? "profile_image_url"
--             <*> o .:? "profile_image_url_https"
--             <*> o .:  "profile_link_color"
--             <*> o .:  "profile_sidebar_border_color"
--             <*> o .:  "profile_sidebar_fill_color"
--             <*> o .:  "profile_text_color"
--             <*> o .:  "profile_use_background_image"
--             <*> o .:  "protected"
--             <*> o .:  "screen_name"
--             <*> o .:? "show_all_inline_media"
--             <*> o .:  "statuses_count"
--             <*> o .:? "time_zone"
--             <*> o .:? "url" .!= Nothing
--             <*> o .:? "utc_offset"
--             <*> o .:  "verified"
--             <*> o .:? "withheld_in_countries"
--             <*> o .:? "withheld_scope"
--    parseJSON v = fail $ "couldn't parse user from: " ++ show v

--instance ToJSON User where
--    toJSON User{..} = object [ "contributors_enabled"               .= userContributorsEnabled
--                             , "created_at"                         .= TwitterTime userCreatedAt
--                             , "default_profile"                    .= userDefaultProfile
--                             , "default_profile_image"              .= userDefaultProfileImage
--                             , "description"                        .= userDescription
--                             , "email"                              .= userEmail
--                             , "favourites_count"                   .= userFavoritesCount
--                             , "follow_request_sent"                .= userFollowRequestSent
--                             , "following"                          .= userFollowing
--                             , "followers_count"                    .= userFollowersCount
--                             , "friends_count"                      .= userFriendsCount
--                             , "geo_enabled"                        .= userGeoEnabled
--                             , "id"                                 .= userId
--                             , "is_translator"                      .= userIsTranslator
--                             , "lang"                               .= userLang
--                             , "listed_count"                       .= userListedCount
--                             , "location"                           .= userLocation
--                             , "name"                               .= userName
--                             , "notifications"                      .= userNotifications
--                             , "profile_background_color"           .= userProfileBackgroundColor
--                             , "profile_background_image_url"       .= userProfileBackgroundImageURL
--                             , "profile_background_image_url_https" .= userProfileBackgroundImageURLHttps
--                             , "profile_background_tile"            .= userProfileBackgroundTile
--                             , "profile_banner_url"                 .= userProfileBannerURL
--                             , "profile_image_url"                  .= userProfileImageURL
--                             , "profile_image_url_https"            .= userProfileImageURLHttps
--                             , "profile_link_color"                 .= userProfileLinkColor
--                             , "profile_sidebar_border_color"       .= userProfileSidebarBorderColor
--                             , "profile_sidebar_fill_color"         .= userProfileSidebarFillColor
--                             , "profile_text_color"                 .= userProfileTextColor
--                             , "profile_use_background_image"       .= userProfileUseBackgroundImage
--                             , "protected"                          .= userProtected
--                             , "screen_name"                        .= userScreenName
--                             , "show_all_inline_media"              .= userShowAllInlineMedia
--                             , "statuses_count"                     .= userStatusesCount
--                             , "time_zone"                          .= userTimeZone
--                             , "url"                                .= userURL
--                             , "utc_offset"                         .= userUtcOffset
--                             , "verified"                           .= userVerified
--                             , "withheld_in_countries"              .= userWithheldInCountries
--                             , "withheld_scope"                     .= userWithheldScope
--                             ]

--data List =
--    List
--    { listId :: Int
--    , listName :: Text
--    , listFullName :: Text
--    , listMemberCount :: Int
--    , listSubscriberCount :: Int
--    , listMode :: Text
--    , listUser :: User
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON List where
--    parseJSON (Object o) = checkError o >>
--        List <$> o .:  "id"
--             <*> o .:  "name"
--             <*> o .:  "full_name"
--             <*> o .:  "member_count"
--             <*> o .:  "subscriber_count"
--             <*> o .:  "mode"
--             <*> o .:  "user"
--    parseJSON v = fail $ "couldn't parse List from: " ++ show v

--instance ToJSON List where
--    toJSON List{..} = object [ "id"                 .= listId
--                             , "name"               .= listName
--                             , "full_name"          .= listFullName
--                             , "member_count"       .= listMemberCount
--                             , "subscriber_count"   .= listSubscriberCount
--                             , "mode"               .= listMode
--                             , "user"               .= listUser
--                             ]

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
instance eqHashTagEntity :: Eq HashTagEntity where eq = genericEq
instance showHashTagEntity :: Show HashTagEntity where show = genericShow

instance decodeJsonHashTagEntity :: DecodeJson HashTagEntity where
  -- decodeJson :: Json -> Either String HashTagEntity
  decodeJson =
    foldJsonObject (Left "not a JObject (HashTagEntity)") \o ->
      toHashTagEntity <$> o .? "text"

instance encodeJsonHashTagEntity :: EncodeJson HashTagEntity where
  -- encodeJson :: HashTagEntity -> Json
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
instance eqUserEntity :: Eq UserEntity where eq = genericEq
instance showUserEntity :: Show UserEntity where show = genericShow

instance decodeJsonUserEntity :: DecodeJson UserEntity where
  -- decodeJson :: Json -> Either String UserEntity
  decodeJson =
    foldJsonObject (Left "not a JObject (UserEntity)") \o ->
      toUserEntity
        <$> o .? "id"
        <*> o .? "name"
        <*> o .? "screen_name"

instance encodeJsonUserEntity :: EncodeJson UserEntity where
  -- encodeJson :: UserEntity -> Json
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
instance eqURLEntity :: Eq URLEntity where eq = genericEq
instance showURLEntity :: Show URLEntity where show = genericShow

instance decodeJsonURLEntity :: DecodeJson URLEntity where
  -- decodeJson :: Json -> Either String URLEntity
  decodeJson =
    foldJsonObject (Left "not a JObject (URLEntity)") \o ->
      toURLEntity
        <$> o .? "url"
        <*> o .? "expanded_url"
        <*> o .? "display_url"

instance encodeJsonURLEntity :: EncodeJson URLEntity where
  -- encodeJson :: URLEntity -> Json
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
instance eqMediaEntity :: Eq MediaEntity where eq = genericEq
instance showMediaEntity :: Show MediaEntity where show = genericShow

instance decodeJsonMediaEntity :: DecodeJson MediaEntity where
  -- decodeJson :: Json -> Either String MediaEntity
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
  -- encodeJson :: MediaEntity -> Json
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
instance eqMediaSize :: Eq MediaSize where eq = genericEq
instance showMediaSize :: Show MediaSize where show = genericShow

instance decodeJsonMediaSize :: DecodeJson MediaSize where
  -- decodeJson :: Json -> Either String MediaSize
  decodeJson =
    foldJsonObject (Left "not a JObject (MediaSize)") \o ->
      toMediaSize
        <$> o .? "w"
        <*> o .? "h"
        <*> o .? "resize"

instance encodeJsonMediaSize :: EncodeJson MediaSize where
  -- encodeJson :: MediaSize -> Json
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
instance eqCoordinates :: Eq Coordinates where eq = genericEq
instance showCoordinates :: Show Coordinates where show = genericShow

instance decodeJsonCoordinates :: DecodeJson Coordinates where
  -- decodeJson :: Json -> Either String Coordinates
  decodeJson =
    foldJsonObject (Left "not a JObject (Coordinates)") \o ->
      toCoordinates
        <$> o .? "coordinates"
        <*> o .? "type"

instance encodeJsonCoordinates :: EncodeJson Coordinates where
  -- encodeJson :: Coordinates -> Json
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
instance eqPlace :: Eq Place where eq = genericEq
instance showPlace :: Show Place where show = genericShow

instance decodeJsonPlace :: DecodeJson Place where
  -- decodeJson :: Json -> Either String Place
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
  -- encodeJson :: Place -> Json
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
toBoundingBox coord t =
  BoundingBox
    { boundingBoxCoordinates: coord
    , boundingBoxType: t
    }

derive instance genericBoundingBox :: Generic BoundingBox _
derive instance newtypeBoundingBox :: Newtype BoundingBox _
instance eqBoundingBox :: Eq BoundingBox where eq = genericEq
instance showBoundingBox :: Show BoundingBox where show = genericShow

instance decodeJsonBoundingBox :: DecodeJson BoundingBox where
  -- decodeJson :: Json -> Either String BoundingBox
  decodeJson =
    foldJsonObject (Left "not a JObject (BoundingBox)") \o ->
      toBoundingBox
        <$> o .? "coordinates"
        <*> o .? "type"

instance encodeJsonBoundingBox :: EncodeJson BoundingBox where
  -- encodeJson :: BoundingBox -> Json
  encodeJson (BoundingBox boundingBox) =
    "coordinates" := boundingBox.boundingBoxCoordinates ~>
    "type" := boundingBox.boundingBoxType ~>
    jsonEmptyObject

---- | Entity handling.
---- See <https://dev.twitter.com/docs/platform-objects/entities>.
--data Entities =
--    Entities
--    { enHashTags     :: [Entity HashTagEntity]
--    , enUserMentions :: [Entity UserEntity]
--    , enURLs         :: [Entity URLEntity]
--    , enMedia        :: [Entity MediaEntity]
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON Entities where
--    parseJSON (Object o) =
--        Entities <$> o .:? "hashtags" .!= []
--                 <*> o .:? "user_mentions" .!= []
--                 <*> o .:? "urls" .!= []
--                 <*> o .:? "media" .!= []
--    parseJSON v = fail $ "couldn't parse entities from: " ++ show v

--instance ToJSON Entities where
--    toJSON Entities{..} = object [ "hashtags"       .= enHashTags
--                                 , "user_mentions"  .= enUserMentions
--                                 , "urls"           .= enURLs
--                                 , "media"          .= enMedia
--                                 ]

---- | The character positions the Entity was extracted from
----
----   This is experimental implementation.
----   This may be replaced by more definite types.
--type EntityIndices = [Int]

--data Entity a =
--    Entity
--    { entityBody    :: a             -- ^ The detail information of the specific entity types (HashTag, URL, User)
--    , entityIndices :: EntityIndices -- ^ The character positions the Entity was extracted from
--    } deriving (Show, Eq, Data, Typeable, Generic)

--instance FromJSON a => FromJSON (Entity a) where
--    parseJSON v@(Object o) =
--        Entity <$> parseJSON v
--               <*> o .: "indices"
--    parseJSON v = fail $ "couldn't parse entity wrapper from: " ++ show v

--instance ToJSON a => ToJSON (Entity a) where
--    toJSON Entity{..} = case toJSON entityBody of
--                            (Object o) -> Object $ union o $ fromList [("indices"::Text, toJSON entityIndices)]
--                            _          -> error "Entity body must produce an object."

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
instance eqContributor :: Eq Contributor where eq = genericEq
instance showContributor :: Show Contributor where show = genericShow

instance decodeJsonContributor :: DecodeJson Contributor where
  -- decodeJson :: Json -> Either String Contributor
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
  -- encodeJson :: Contributor -> Json
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
instance eqImageSizeType :: Eq ImageSizeType where eq = genericEq
instance showImageSizeType :: Show ImageSizeType where show = genericShow

instance decodeJsonImageSizeType :: DecodeJson ImageSizeType where
  -- decodeJson :: Json -> Either String ImageSizeType
  decodeJson =
    foldJsonObject (Left "not a JObject (ImageSizeType)") \o ->
      toImageSizeType
        <$> o .? "w"
        <*> o .? "h"
        <*> o .? "image_type"

instance encodeJsonImageSizeType :: EncodeJson ImageSizeType where
  -- encodeJson :: ImageSizeType -> Json
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
instance eqUploadedMedia :: Eq UploadedMedia where eq = genericEq
instance showUploadedMedia :: Show UploadedMedia where show = genericShow

instance decodeJsonUploadedMedia :: DecodeJson UploadedMedia where
  -- decodeJson :: Json -> Either String UploadedMedia
  decodeJson =
    foldJsonObject (Left "not a JObject (UploadedMedia)") \o ->
      toUploadedMedia
        <$> o .? "media_id"
        <*> o .? "size"
        <*> o .? "image"

instance encodeJsonUploadedMedia :: EncodeJson UploadedMedia where
  -- encodeJson :: UploadedMedia -> Json
  encodeJson (UploadedMedia uploadedMedia) =
    "media_id" := uploadedMedia.uploadedMediaId ~>
    "size" := uploadedMedia.uploadedMediaSize ~>
    "image" := uploadedMedia.uploadedMediaImage ~>
    jsonEmptyObject
