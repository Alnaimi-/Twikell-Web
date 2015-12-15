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

-- | Takes a list of tweets and renders it to json
--   for display to the user.
listedTweets :: [Tweet] -> ActionM ()
listedTweets tweets = json tweets

-- | Takes a Maybe Tweet. And renders it to JSON
viewTweet :: Maybe Tweet -> ActionM ()
viewTweet Nothing = json (TL.pack "Error retreiving tweets. Is the Tweet ID correct?") -- ^ Case of nothing, return error
viewTweet (Just tweet) = json tweet -- ^ Otherwise return the tweet

-- | Takes a Maybe list of tweets and renders all of them.
insertedTweets :: Maybe [Tweet] -> ActionM ()
insertedTweets tweets = json tweets

-- | Displays message for deleting a tweet.
deletedTweet :: Maybe Integer -> ActionM ()
deletedTweet Nothing = json (TL.pack "Error deleting tweet. Is the tweet ID correct?") -- ^ Tweet was not deleted sucessfully.
deletedTweet _ = json (TL.pack "Deleted tweet") -- ^ Display message informing the user

{- Following is concerned with displaying the User -}

-- | Takes a Maybe user and renders it to json 
--   for display to the user. Erro thrown if
--   the cURL or PUT request was erroneous. 
updatedUser :: Maybe User -> ActionM ()
updatedUser Nothing = json (TL.pack "Error updating user. Verify your cURL request.")
updatedUser user = json user

-- | Takes a Maybe user and does same as above
--   however the error here is thrown if user not found.
viewUser :: Maybe User -> ActionM ()
viewUser Nothing = json (TL.pack "Error retrieving user. Does the user exist?")
viewUser (Just user) = json user