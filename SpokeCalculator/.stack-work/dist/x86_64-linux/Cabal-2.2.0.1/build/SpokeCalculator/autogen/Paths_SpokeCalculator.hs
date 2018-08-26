{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SpokeCalculator (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mm/learningHaskell/SpokeCalculator/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/bin"
libdir     = "/home/mm/learningHaskell/SpokeCalculator/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/lib/x86_64-linux-ghc-8.4.3/SpokeCalculator-0.1.0.0-HDDqZ3jF2RjDLWMwthrZMr-SpokeCalculator"
dynlibdir  = "/home/mm/learningHaskell/SpokeCalculator/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/mm/learningHaskell/SpokeCalculator/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/share/x86_64-linux-ghc-8.4.3/SpokeCalculator-0.1.0.0"
libexecdir = "/home/mm/learningHaskell/SpokeCalculator/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/libexec/x86_64-linux-ghc-8.4.3/SpokeCalculator-0.1.0.0"
sysconfdir = "/home/mm/learningHaskell/SpokeCalculator/.stack-work/install/x86_64-linux/lts-12.7/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SpokeCalculator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SpokeCalculator_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SpokeCalculator_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SpokeCalculator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SpokeCalculator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SpokeCalculator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
