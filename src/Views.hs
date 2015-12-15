{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Views where

import JSON

import GHC.Generics (Generic)
import Web.Scotty
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class
import Web.Scotty.Internal.Types

{- Following is concerned with displaying the Tweet -}

listedTweets :: [Tweet] -> ActionM ()
listedTweets tweets = json tweets

viewTweet :: Maybe Tweet -> ActionM ()
viewTweet Nothing = json (TL.pack "Error retreiving tweets. Is the Tweet ID correct?")
viewTweet (Just tweet) = json tweet

insertedTweets :: Maybe [Tweet] -> ActionM ()
insertedTweets tweets = json tweets

deletedTweet :: TL.Text -> ActionM ()
deletedTweet id = json (TL.pack "Deleted tweet by id: " `TL.append` id)

{- Following is concerned with displaying the User -}

updatedUser :: Maybe User -> ActionM ()
updatedUser Nothing = json (TL.pack "Error updating user. Verify your cURL request.")
updatedUser user = json user

viewUser :: Maybe User -> ActionM ()
viewUser Nothing = json (TL.pack "Error retrieving user. Does the user exist?")
viewUser (Just user) = json user