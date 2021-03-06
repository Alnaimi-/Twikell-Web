{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Auth
Description : Auth module for sensitive data
Maintainer  : al.alnaimi@gmail.com

This module handles the verification of authenticating
a user from accessing sensitive routes in the framework
such as POST and DELETE requests.
-}

module Auth where

import Db

import Data.String
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Pool(Pool, createPool, withResource)
import Database.MySQL.Simple
import Data.Hash.MD5

-- | Encodes provided password with md5 and then compares it with the hash
--   stored in the DB to verify a senstive request.
verifyCredentials :: Pool Connection -> B.ByteString -> B.ByteString -> IO Bool
verifyCredentials pool user password = do
   pwd <- findUserByLogin pool (BC.unpack user)
   return $ comparePasswords pwd (BC.unpack password)
   where comparePasswords Nothing _ = False
         comparePasswords (Just p) password =  p == (md5s $ Str password)


