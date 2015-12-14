{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views where

import JSON

import GHC.Generics (Generic)
import Web.Scotty
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad.IO.Class
import Web.Scotty.Internal.Types

{- Following is concerned with displaying the Tweet -}

listedTweets :: [Tweet] -> ActionM ()
listedTweets tweets = json tweets

viewTweet :: Maybe Tweet -> ActionM ()
viewTweet Nothing = json ()
viewTweet (Just tweet) = json tweet

insertedTweets :: Maybe [Tweet] -> ActionM ()
insertedTweets tweets = json tweets

deletedTweet :: TL.Text -> ActionM ()
deletedTweet id = json ()

{- Following is concerned with displaying the User -}

updatedUser :: Maybe User -> ActionM ()
updatedUser article = json ()

viewUser :: Maybe User -> ActionM ()
viewUser Nothing = json ()
viewUser (Just user) = json user