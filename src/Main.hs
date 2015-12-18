{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Main entry for the framework
Maintainer  : al.alnaimi@gmail.com
Stability   : experimental
Portability : POSIX

This module handles HTTP request using the 
Wai server framework and Scotty for routing.
-}

module Main where

import Auth
import Db
import JSON
import RESTApi
import Views

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.MySQL.Simple
import Data.Maybe
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import Data.Aeson

-- | Parse file "application.conf" and construct a databse
--   connection from it.
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

-- | The function returns whether if a resource in a path
--   is restricted to authenticated users.
protectedResources ::  Request -> IO Bool
protectedResources request = do
  let path = pathInfo request
  return $ protect path
  where protect (p : _) =  p == "admin"  -- all requests to /admin/* should be authenticated
        protect _       =  False         -- other requests are allowed for anonymous users

-- | The main entry to configure database
--   port and routing
main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf
    
  case dbConf of
    Nothing -> putStrLn "No database configuration found, terminating..."
    Just conf -> do      
      pool <- createPool (newConn conf) close 1 64 10
      scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "static")               -- serve static files
        middleware $ logStdout                                                -- log all requests; for production use logStdout
        middleware $ basicAuth (verifyCredentials pool)                       -- check if the user is authenticated for protected resources
          "Twikell Indulge Yourself" { authIsProtected = protectedResources } -- function which restricts access to some routes only for authenticated users

        {- Following is concerned with handling Tweets -}

        -- LIST
        get    "/tweet" $ do 
          tweets <- liftIO $ listTweets pool "%%"                   -- get the list of ALL tweets from DB
          listedTweets tweets                                       -- show tweet list

        -- VIEW
        get (regex "/tweet/(\\w*)") $ do 
          name <- param "1" :: ActionM TL.Text                      -- get the screen_name from the request
          tweets <- liftIO $ listTweets pool name                   -- get tweet(s) from the DB belonging to name
          listedTweets tweets                                       -- show the tweet list

        -- VIEW
        get (regex "/tweetid/([0-9]*)") $ do 
          id <- param "1" :: ActionM TL.Text                        -- get the tweet id from the request
          maybeTweet <- liftIO $ findTweet pool id                  -- get the tweet from the DB
          viewTweet maybeTweet                                      -- show the tweet if it was found

        -- CREATE
        post   "/admin/tweet/:name" $ do 
          name <- param "name" :: ActionM TL.Text                   -- read the screen_name
          tweets <- liftIO $ getTimeline $ TL.unpack name           -- Retrieve tweets for said screen name from Twitter
          insertTweets pool tweets                                  -- insert parsed tweets into the DB
          listedTweets tweets                                       -- Echo the tweets retrieved from Twitter, using same view
        
        -- DELETE
        -- Using param passing here instead just to demonstrate
        delete "/admin/tweet" $ do 
          id <- param "id" :: ActionM TL.Text                       -- get the tweet id
          maybeDeleted <- liftIO $ deleteTweet pool id              -- delete the tweet from the DB
          deletedTweet maybeDeleted                                 -- show info that the tweet was deleted

        {- Following is concerned with handling Users -}

        -- LIST
        get    "/user" $ do 
          users <- liftIO $ listUsers pool "%%"                     -- get the list of ALL users from DB
          listedUsers users                                         -- show user list

        -- VIEW
        get    "/user/:name" $ do 
          name <- param "name" :: ActionM TL.Text                   -- get the screen_name from the request
          maybeUser <- liftIO $ listUsers pool name                 -- get the user from the DB, using same method as listUsers
          listedUsers maybeUser                                     -- show the user if he was found
        
        -- CREATE
        post   "/admin/user/:name" $ do 
          name <- param "name" :: ActionM TL.Text                   -- get the screen_name(s) from the request
          users <- liftIO $ getUsers $ TL.unpack name               -- get the user(s) from Twitter, as Haskell [User]
          insertUsers pool users                                    -- insert the users into Database.
          listedUsers users                                         -- Echo the user's data retrieved from Twitter
        
        -- UPDATE
        put    "/admin/user" $ do 
          user <- getUserParam                                      -- read the request body of type JSON & parse it
          updateUser pool user                                      -- update parsed user in the DB
          updatedUser user                                          -- show info that the user was updated

        -- GET
        get    "/search/:name" $ do
          query <- param "name" :: ActionM TL.Text                  -- get the param name representing search query
          users <- liftIO $ searchUsers $ TL.unpack query           -- send a GET request to the Twitter API
          listedUsers users                                         -- List all users matching query
                                                                    -- PS this doesn't add resulted users to DB

----------------------------------------------

-- | Parse the request body of type JSON, returning User object
getUserParam :: ActionT TL.Text IO (Maybe User)
getUserParam = do b <- body
                  return $ (decode b :: Maybe User)