module Paths_hblog_scotty (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bloo/.cabal/bin"
libdir     = "/home/bloo/.cabal/lib/x86_64-linux-ghc-7.10.2.20151118/hblog-scotty-0.1.0.0-E07BcYQSx7LHPtaSkmqbGC"
datadir    = "/home/bloo/.cabal/share/x86_64-linux-ghc-7.10.2.20151118/hblog-scotty-0.1.0.0"
libexecdir = "/home/bloo/.cabal/libexec"
sysconfdir = "/home/bloo/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hblog_scotty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hblog_scotty_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hblog_scotty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hblog_scotty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hblog_scotty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
