module Exceptions (myHttpException) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status   (statusCode)
import qualified Control.Exception as E

-- Most error handling was done through Maybe
-- As such type of handling allowed me to be
-- flexible with the front-end displaying 
-- errors which should be visible to the user
--
-- Exceptions are more for the back-end. 
-- Handling errors that are and should be
-- transparent to the end user.
--
-- | HTTP StatusCode Exception handler
myHttpException :: HttpException -> IO (Maybe a)
myHttpException (StatusCodeException s _ _) = 
  putStrLn ((show . statusCode $ s) ++ " Exception has occured. Check the URL!") >> return Nothing