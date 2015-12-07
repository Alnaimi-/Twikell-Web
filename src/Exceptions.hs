module Exceptions (myHttpException) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status   (statusCode)
import qualified Control.Exception as E

-- | HTTP StatusCode Exception handler
myHttpException :: HttpException -> IO (Maybe a)
myHttpException (StatusCodeException s _ _) = 
  putStrLn ((show . statusCode $ s) ++ " Exception has occured. Check the URL!") >> return Nothing