{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_pacman (
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

bindir     = "C:\\Users\\Steven Heinen\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Steven Heinen\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\haskell-pacman-0.1.0.0-3dC1njWbBKMXTDJP0ZRu0"
dynlibdir  = "C:\\Users\\Steven Heinen\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\Steven Heinen\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\haskell-pacman-0.1.0.0"
libexecdir = "C:\\Users\\Steven Heinen\\AppData\\Roaming\\cabal\\haskell-pacman-0.1.0.0-3dC1njWbBKMXTDJP0ZRu0\\x86_64-windows-ghc-8.2.1\\haskell-pacman-0.1.0.0"
sysconfdir = "C:\\Users\\Steven Heinen\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_pacman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_pacman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_pacman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_pacman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_pacman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_pacman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
