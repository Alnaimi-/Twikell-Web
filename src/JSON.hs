{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON where

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

-- | The User type, given by above retreived Tweets.
data User = User {
  screenName    :: TL.Text,
  name          :: TL.Text,
  image         :: TL.Text,
  location      :: TL.Text,
  followerCount :: Integer
} deriving (Show, Generic)

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "screen_name"
         <*> v .: "name"
         <*>v  .: "profile_image_url"
         <*> v .: "location"
         <*> v .: "followers_count"

-- | Type constructor for Users, representing a 
--   [User]. This is because the follower request
--   for twitter returns a "Users":[{},{}] JSON.
data Users = Users [User] deriving (Show, Generic)

instance FromJSON Users where
  parseJSON (Object v) =
    Users <$> v .: "users"

-- | Type constructor for tags, which simply consists
--   of a Text object.
data Tag = Tag TL.Text deriving (Show, Generic)

instance FromJSON Tag where
  parseJSON (Object v) = 
    Tag <$> v .: "text"