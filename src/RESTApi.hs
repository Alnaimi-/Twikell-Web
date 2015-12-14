{-# LANGUAGE OverloadedStrings #-}

module RESTApi (
  timeline,
  followers,
  searchUser
) where

import Exceptions
import JSON

import Data.ByteString (ByteString)
import qualified Control.Exception as E
import Data.Maybe
import Data.Aeson
import Network.HTTP.Conduit
import Web.Authenticate.OAuth

-- | Authentication for the Auth Twitter API
myoauth :: OAuth
myoauth = newOAuth {
  oauthServerName     = "api.twitter.com",
  oauthConsumerKey    = "BXaPjgEW6nCI2BxN80ioCq0HY",
  oauthConsumerSecret = "SBljiPIYVQoGXZrEoe7xhfMTYRQ6mDq3t4ZqeWgdmH3uhizodD"
}

-- | Credentials for the Developer App
mycred :: Credential
mycred = newCredential "581290446-nzMy19LHcxGsxV3KeLoTPtGMOYrdsrXZus9RSdD5"
                       "B2oHnLcCqZV1fKPqdozDeO6zZ9l2D2Qds3yWFae2foGMR"

-- | Function taking a Request object and Returns a FromJSON haskell equiv.
decodeRequest :: FromJSON a => Request -> IO (Maybe a)
decodeRequest req = do
  -- OAuth Authentication. 'signOAuth' modifies the HTTP header adding the
  -- appropriate authentication.
  signedreq <- signOAuth myoauth mycred req

  -- Using a HTTP manager, we authenticate the request and send it to get a response.
  manager <- newManager tlsManagerSettings

  -- Apply Just to the response. catch any status exception.
  res <- (Just <$> httpLbs signedreq manager) `E.catch` myHttpException

  -- Decode the response body, unwrapping the Maybe.
  case res of 
    Nothing   -> return Nothing
    otherwise -> return $ decode $ responseBody $ fromJust res


-------------------------- Twitter API Querying Methods --------------------------
----------------------------------------------------------------------------------

-- | This function reads a timeline JSON and parse it using the 'Tweet' type.
timeline :: String -- ^ Screen name of the user
         -> IO (Maybe [Tweet]) -- ^ If there's an error, parsing the JSON then
                               --   return Left with the String, Else [Tweet] obj.
timeline name = do
  -- Firstly, we create a HTTP request with method GET.
  -- This particular request searches for a screen name, and retreives their timeline.
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name

  -- Tweet object parsed by decodeRequest method.
  decodeRequest req -- ^ passing the Request object.

-- | This function retrieves all followers of a certain user. Up to 200.
followers :: String -- ^ Screen name of the user
          -> IO (Maybe Users)
followers name = do
  -- Create a HTTP request with GET; in this scenario we are searching registered users 
  req <- parseUrl $ "https://api.twitter.com/1.1/followers/list.json?cursor=-1&screen_name=" ++ name ++ "&count=50"

  -- List of followers returned by decoding the Request
  decodeRequest req

-- | This function takes a search query and returns all Users matching query.
searchUser :: String -- ^ Search query for user
           -> IO (Maybe [User]) -- ^ Just like timeline, this time we return [User]
searchUser query = do 
  -- Create a HTTP request with GET; in this scenario we are searching registered users 
  req <- parseUrl $ "https://api.twitter.com/1.1/users/search.json?q=" ++ query ++ "&page=1&count=3"

  decodeRequest req

----------------------------------------------------------------------------------
---------------------------------------------------------------------------------- 