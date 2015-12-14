{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where
import JSON

import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Data.Maybe
import qualified Database.MySQL.Base as M
import Database.MySQL.Simple
import Database.MySQL.Simple.Types
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.QueryParams
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import GHC.Int

-- DbConfig contains info needed to connect to MySQL server
data DbConfig = DbConfig {
  dbName     :: String,
  dbUser     :: String,
  dbPassword :: String
} deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo { 
  connectUser     = dbUser conf,
  connectPassword = dbPassword conf,
  connectDatabase = dbName conf
}

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (QueryResults r, QueryParams q) => Pool M.Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
  where retrieve conn = query conn sql args

-- No arguments -- just pure sql
fetchSimple :: QueryResults r => Pool M.Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
  where retrieve conn = query_ conn sql

-- Update database
execSql :: QueryParams q => Pool M.Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
  where ins conn = execute conn sql args

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--
-- Accepts arguments
fetchT :: (QueryResults r, QueryParams q) => Pool M.Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
  where retrieve conn = withTransaction conn $ query conn sql args

-- No arguments -- just pure sql
fetchSimpleT :: QueryResults r => Pool M.Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
  where retrieve conn = withTransaction conn $ query_ conn sql

-- Update database
execSqlT :: QueryParams q => Pool M.Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
  where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------

findUserByLogin :: Pool Connection -> String -> IO (Maybe String)
findUserByLogin pool login = do
  res <- liftIO $ fetch pool (Only login) "SELECT * FROM user WHERE login=?" :: IO [(Integer, String, String)]
  return $ password res
  where password [(_, _, pwd)] = Just pwd
        password _ = Nothing

--------------------------------------------------------------------------------

{- Following methods are concerned with the tweet table -}

listTweets :: Pool Connection -> IO [Tweet]
listTweets pool = do
  res <- fetchSimple pool 
         "SELECT * FROM tweet INNER JOIN twitter_user ON tweet.name=twitter_user.screen_name ORDER BY id DESC" 
         :: IO [(Integer, TL.Text, Integer, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, Integer)]
  
  return $ map (\(tweetId, text, reCount, tags, user, scrn, name, img, loc, followerC) -> 
                Tweet tweetId text reCount (getTags tags) (User scrn name img loc followerC)) res
  where getTags tags = map (\tag -> Tag tag) $ TL.splitOn (TL.pack ",") tags

findTweet :: Pool Connection -> TL.Text -> IO (Maybe Tweet)
findTweet pool id = do
  res <- fetch pool (Only id) 
         "SELECT * FROM tweet INNER JOIN twitter_user ON tweet.name=twitter_user.screen_name WHERE id=?"
         :: IO [(Integer, TL.Text, Integer, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, Integer)]

  return $ oneTweet res
  where oneTweet ((tweetId, text, reCount, tags, user, scrn, name, img, loc, followerC) : _) = 
                  Just $ Tweet tweetId text reCount (getTags tags) (User scrn name img loc followerC)
        oneTweet _ = Nothing
        getTags tags = map (\tag -> Tag tag) $ TL.splitOn (TL.pack ",") tags

insertTweets :: Pool Connection -> Maybe [Tweet] -> ActionT TL.Text IO ()
insertTweets pool Nothing = return ()
insertTweets pool (Just tweets) = do
  a <- mapM_ (insertTweet pool) tweets
  return ()

insertTweet :: Pool Connection -> Tweet -> ActionT TL.Text IO ()
insertTweet pool tweet = do
  let tweetId' = fromInteger $ tweetId tweet
  let reCount' = fromInteger $ reCount tweet
  let hashtags' = compactForm $ hashtags tweet
  let user' = screenName (user tweet)

  liftIO $ execSqlT pool [tweetId', text tweet, reCount', hashtags', user']
                         "INSERT IGNORE INTO tweet(id, tweet, reCount, hashtags, name) VALUES(?,?,?,?,?)"
  return ()
  where fromInteger a = TL.pack $ show $ a
        compactForm a = TL.intercalate ("," :: TL.Text) $ map (\(Tag h) -> h) $ a

deleteTweet :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deleteTweet pool id = do
  liftIO $ execSqlT pool [id] "DELETE FROM tweet WHERE id=?"
  return ()                      

{- Following methods are concerned with the user table -}

findUser :: Pool Connection -> TL.Text -> IO (Maybe User)
findUser pool name = do
  res <- fetch pool (Only name) "SELECT * FROM twitter_user WHERE screen_name=?" :: IO [(TL.Text, TL.Text, TL.Text, TL.Text, Integer)]

  return $ oneUser res
  where oneUser ((scrn, name, img, loc, followerC) : _) = Just $ User scrn name img loc followerC
        oneUser _ = Nothing

updateUser :: Pool Connection -> Maybe User -> ActionT TL.Text IO ()
updateUser pool Nothing = return ()
updateUser pool (Just (User a b c d e)) = do
  --liftIO $ execSqlT pool [title, bodyText, (TL.decodeUtf8 $ BL.pack $ show id)]
  --                       "UPDATE tweet SET text=?, retweet_count=? WHERE id=?"
  return ()