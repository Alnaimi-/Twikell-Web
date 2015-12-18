{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db where
import JSON
import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import qualified Control.Exception as E
import Control.Monad 
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

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- These methods retrieves the connection through the resource pool
-- And fetches/executes results transactionally for fault tolerance

-- | Fetch command which accepts arguments
fetchT :: (QueryResults r, QueryParams q) => Pool M.Connection -> q -> Query -> IO [r]
fetchT pool args sql = withResource pool retrieve
  where retrieve conn = withTransaction conn $ query conn sql args

-- | Simple fetch without prepared arguments
fetchSimpleT :: QueryResults r => Pool M.Connection -> Query -> IO [r]
fetchSimpleT pool sql = withResource pool retrieve
  where retrieve conn = withTransaction conn $ query_ conn sql

-- | Execute with arguments to update database
execSqlT :: QueryParams q => Pool M.Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
  where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------

-- | Returns the encrypted password for a user from the DB
findUserByLogin :: Pool Connection -> String -> IO (Maybe String)
findUserByLogin pool login = do
  res <- liftIO $ fetchT pool (Only login) "SELECT * FROM user WHERE login=?" :: IO [(Integer, String, String)]
  return $ password res
  where password [(_, _, pwd)] = Just pwd
        password _ = Nothing

--------------------------------------------------------------------------------

{- Following methods are concerned with the tweet table -}

-- | Lists all tweets matching a certain query
listTweets :: Pool Connection -> TL.Text -> IO (Maybe [Tweet])
listTweets pool name = do
  res <- fetchT pool (Only name)
           -- Preform an inner join on the two tables; returning associated user for each tweet.
           "SELECT * FROM tweet INNER JOIN twitter_user ON tweet.name=twitter_user.screen_name WHERE screen_name LIKE ? ORDER BY id DESC"
           :: IO [(Integer, TL.Text, Integer, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, Integer)]

  -- Map the parseTweet function on the result of fetch
  return $ isEmpty $ map parseTweet res

  where getTags tags = map (\tag -> Tag tag) $ TL.splitOn (TL.pack ",") tags            -- ^ Splits the tags saved as X,Y,Z 
        parseTweet (tweetId, text, reCount, tags, user, scrn, name, img, loc, flwrC) =  --   and reconstruct into Tag type
          Tweet tweetId text reCount (getTags tags) (User scrn name img loc flwrC)      -- ^ Reconstruct a tweet from the fetchedresults.
        isEmpty [] = Nothing
        isEmpty xs = Just $ xs  

-- | List a single tweet based on user ID
findTweet :: Pool Connection -> TL.Text -> IO (Maybe Tweet)
findTweet pool id = do
  res <- fetchT pool (Only id)
           -- Prepare the statement and set the id = ? to the id passed
           "SELECT * FROM tweet INNER JOIN twitter_user ON tweet.name=twitter_user.screen_name WHERE id=?"
           :: IO [(Integer, TL.Text, Integer, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, TL.Text, Integer)]

  return $ oneTweet res

  where getTags tags = map (\tag -> Tag tag) $ TL.splitOn (TL.pack ",") tags                           -- Same as tags above
        oneTweet ((tweetId, text, reCount, tags, user, scrn, name, img, loc, flwrC) : _) =         -- ^ Here we only reconstruct the first tweet as 
                  Just $ Tweet tweetId text reCount (getTags tags) (User scrn name img loc flwrC)  --   there shouldn't be more with same unique id
        oneTweet _ = Nothing                                                                           -- If tweet not fond return Nothing

-- | Insert list of tweets into database based on returned tweets from
--   Querying the REST API of twitter
insertTweets :: Pool Connection -> Maybe [Tweet] -> ActionT TL.Text IO ()
insertTweets pool Nothing = return ()      -- If no tweets were returned, then return empty
insertTweets pool (Just tweets) = do       -- If tweets were returned by the REST request
  -- mapM_ as it shouldn't return anyhting
  -- Insert each tweet in the list.
  a <- mapM_ (insertTweet pool) tweets
  return ()

-- | Insert a tweet into the database
insertTweet :: Pool Connection -> Tweet -> ActionT TL.Text IO ()
insertTweet pool tweet = do
  let tweetId' = TL.pack $ show $ tweetId tweet  -- From Integer to TL.Text
  let reCount' = TL.pack $ show $ reCount tweet  -- ^
  let hashtags' = compactForm $ hashtags tweet   -- Intercalate a tweet from [Tag x, Tag y] --> x,y
  let user' = user tweet                         -- get user object
  let scrn' = screenName user'                   -- get the screen_name

  -- Insert user unless already exist
  insertUser pool user'

  -- Prepare statement & insert the individual tweet to the DB, ON Duplicate, do something meaningless
  -- Alternative is INSERT IGNORE; which ignores ALL errors!
  liftIO $ execSqlT pool [tweetId', text tweet, reCount', hashtags', scrn']
             "INSERT INTO tweet(id, tweet, reCount, hashtags, name) VALUES(?,?,?,?,?) ON DUPLICATE KEY UPDATE id=id"

  return ()
  where compactForm a = TL.intercalate ("," :: TL.Text) $ map (\(Tag h) -> h) $ a
        exists (x:_) = True
        exists _     = False

-- | Delete a tweet in the database based on id
deleteTweet :: Pool Connection -> TL.Text -> IO (Maybe Integer)
deleteTweet pool id = do
  res <- liftIO $ execSqlT pool [id] "DELETE FROM tweet WHERE id=?"
  
  return $ oneRes res
  where oneRes 0 = Nothing    -- If no rows affected, return Nothing
        oneRes _ = Just 1     -- Else return a dummy value

{- Following methods are concerned with the user table -}

-- | Lists all users matching a certain query
listUsers :: Pool Connection -> TL.Text -> IO (Maybe [User])
listUsers pool name = do
  res <- fetchT pool (Only name)
           -- Preform an inner join on the two tables; returning associated user for each tweet.
           "SELECT * FROM twitter_user WHERE screen_name LIKE ?" -- The LIKE allows us to get all users
           :: IO [(TL.Text, TL.Text, TL.Text, TL.Text, Integer)] -- Or a single user, by using same method!
  
  -- Map the parseTweet function on the result of fetch
  return $ isEmpty $ map parseUser res 

  where parseUser (scrn, name, img, loc, flwrC) = User scrn name img loc flwrC
        isEmpty [] = Nothing   -- If no users found matching query, return Nothing
        isEmpty xs = Just $ xs -- Else return Just the users

-- | Insert list of users into database based on returned users from
--   Querying the REST API of twitter
insertUsers :: Pool Connection -> Maybe [User] -> ActionT TL.Text IO ()
insertUsers pool Nothing = return ()        -- If no user was found, return Nothing
insertUsers pool (Just users) = do 
  -- mapM_ as it shouldn't return anyhting
  -- Insert each user in the list.
  a <- mapM_ (insertUser pool) users
  return ()

-- | Insert a user into the database
insertUser :: Pool Connection -> User -> ActionT TL.Text IO ()
insertUser pool user = do     
  let scrn'  = screenName user         
  let name'  = name user 
  let img'   = image user
  let loc'   = location user
  let flwrC' = TL.pack $ show $ followerCount user
  
  -- Again if user already exists, then do something meaningless.
  liftIO $ execSqlT pool [scrn', name', img', loc', flwrC']
             "INSERT INTO twitter_user(screen_name, name, image, location, followers) VALUES(?,?,?,?,?) ON DUPLICATE KEY UPDATE name=name"
  -- Rewrap into Just user to display
  return ()

-- | Update a user based on unique screen_name
updateUser :: Pool Connection -> Maybe User -> ActionT TL.Text IO ()
updateUser pool Nothing = return ()
updateUser pool (Just (User scrn name img loc flwrC)) = do
  -- Prepare statement and set each value of user
  liftIO $ execSqlT pool [name, img, loc, (TL.pack $ show flwrC), scrn] -- Last bit is from Integer -> TL.Text
             "UPDATE twitter_user SET name=?, image=?, location=?, followers=? WHERE screen_name=?"
  return ()