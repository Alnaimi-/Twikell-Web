{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Control.Applicative
import Data.Aeson
import GHC.Generics
import qualified Data.Text.Lazy as TL

-- | The Tweet type, contains only fields I am
--   Interested in.
data Tweet = Tweet {
  tweetId  :: Integer,
  text     :: TL.Text,
  reCount  :: Integer,
  hashtags :: [Tag],
  user     :: User
} deriving (Show, Generic)

-- | Specify the parsing of Tweet obj. For instance 
--   here we we are drilling inwards to grab nested obj.
instance FromJSON Tweet where
  parseJSON (Object v) =
    Tweet <$> v .: "id"
          <*> v .: "text"
          <*> v .: "retweet_count"
          <*> ((v .: "entities") >>= (.: "hashtags")) -- ^ Parsing hashtag is given by entities.
          <*> v .: "user"                             --   Each tag is then extracted acc to Tag.

-- | Specify the parsing of Tweet object to JSON
instance ToJSON Tweet where
     toJSON (Tweet tweetId text reCount hashtags user) =
         object ["id"            .= tweetId,
                 "text"          .= text,
                 "retweet_count" .= reCount,
                 "hashtags"      .= hashtags,
                 "user"          .= user]

-- | The User type, given by above retreived Tweets.
data User = User {
  screenName    :: TL.Text,
  name          :: TL.Text,
  image         :: TL.Text,
  location      :: TL.Text,
  followerCount :: Integer
} deriving (Show, Generic)

-- | Specify the parsing of User objects to JSON
instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "screen_name"
         <*> v .: "name"
         <*> v .: "profile_image_url"
         <*> v .: "location"
         <*> v .: "followers_count"

-- | Specify the parsing of User object to JSON
instance ToJSON User where
     toJSON (User screenName name image location followerCount) =
         object ["screen_name"    .= screenName,
                 "name"           .= name,
                 "image"          .= image,
                 "location"       .= location,
                 "follower_count" .= followerCount]

-- | Type constructor for tags, which simply consists
--   of a Text object.
data Tag = Tag TL.Text deriving (Show, Generic)

-- | Specify the parsing of Tag object from JSON
instance FromJSON Tag where
  parseJSON (Object v) = 
    Tag <$> v .: "text"

-- | Specify the parsing of Tag object to JSON
instance ToJSON Tag where
     toJSON (Tag tag) =
         object ["tag" .= tag]