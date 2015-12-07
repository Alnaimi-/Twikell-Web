{-# LANGUAGE OverloadedStrings #-}

module Main where

import Db
import Views
import Auth
import Domain
import RESTApi

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

-- Parse file "application.conf" and get the DB connection info
makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password

-- The function knows which resources are available only for the
-- authenticated users
protectedResources ::  Request -> IO Bool
protectedResources request = do
  let path = pathInfo request
  return $ protect path
  where protect (p : _) =  p == "admin"  -- all requests to /admin/* should be authenticated
        protect _       =  False         -- other requests are allowed for anonymous users


main :: IO ()
main = do
  loadedConf <- C.load [C.Required "application.conf"]
  dbConf <- makeDbConfig loadedConf
    
  case dbConf of
    Nothing -> putStrLn "No database configuration found, terminating..."
    Just conf -> do      
      pool <- createPool (newConn conf) close 1 64 10
      scotty 3000 $ do
        middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
        middleware $ logStdout                                  -- log all requests; for production use logStdout
        middleware $ basicAuth (verifyCredentials pool)         -- check if the user is authenticated for protected resources
                       "Haskell Blog Realm" { authIsProtected = protectedResources } -- function which restricts access to some routes only for authenticated users

        {- Following is concerned with handling Tweets -}

        -- LIST
        get    "/tweets" $ do tweets <- liftIO $ listTweets pool               -- get the ist of tweets for DB
                              listedTweets tweets                              -- show tweet list
        
        -- VIEW
        get    "/tweets/:id" $ do id <- param "id" :: ActionM TL.Text          -- get the article id from the request
                                  maybeArticle <- liftIO $ findArticle pool id -- get the article from the DB
                                  viewArticle maybeArticle                     -- show the article if it was found

        -- CREATE
        post   "/admin/tweet" $ do name <- param "name" :: ActionM TL.Text     -- read the screen_name
                                   tweets <- liftIO $ timeline "katyperry"     -- Retrieve tweets for said screen name
                                   insertTweets pool tweets                    -- insert parsed tweets into the DB
                                   insertedTweets tweets                       -- show info that the tweet was added

        -- DELETE
        delete "/admin/tweet/:id" $ do id <- param "id" :: ActionM TL.Text     -- get the tweet id
                                       deleteTweet pool id                     -- delete the tweet from the DB
                                       deletedTweet id                         -- show info that the tweet was deleted

        {- Following is concerned with handling Users -}
        -- VIEW
        get    "/user/:name" $ do name <- param "name" :: ActionM TL.Text      -- get the screen_name from the request
                                  maybeUser <- liftIO $ findUser pool name     -- get the user from the DB
                                  viewUser maybeUser                           -- show the user if it was found
        -- UPDATE
        put    "/admin/user" $ do tweet <- getArticleParam                     -- read the request body, try to parse it into article
                                  updateUser pool tweet                        -- update parsed article in the DB
                                  updatedUser tweet                            -- show info that the article was updated

----------------------------------------------

-- Parse the request body into the Article
getArticleParam :: ActionT TL.Text IO (Maybe Article)
getArticleParam = do b <- body
                     return $ (decode b :: Maybe Article)
                     where makeArticle s = ""