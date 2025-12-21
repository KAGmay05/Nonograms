{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_nonograms (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\kelen\\Documents\\GitHub\\Nonograms\\.stack-work\\install\\c51e1627\\bin"
libdir     = "C:\\Users\\kelen\\Documents\\GitHub\\Nonograms\\.stack-work\\install\\c51e1627\\lib\\x86_64-windows-ghc-9.10.3-b42a\\nonograms-0.1.0.0-8Q52qArW5t6KcYffpJoYoI-nonograms"
dynlibdir  = "C:\\Users\\kelen\\Documents\\GitHub\\Nonograms\\.stack-work\\install\\c51e1627\\lib\\x86_64-windows-ghc-9.10.3-b42a"
datadir    = "C:\\Users\\kelen\\Documents\\GitHub\\Nonograms\\.stack-work\\install\\c51e1627\\share\\x86_64-windows-ghc-9.10.3-b42a\\nonograms-0.1.0.0"
libexecdir = "C:\\Users\\kelen\\Documents\\GitHub\\Nonograms\\.stack-work\\install\\c51e1627\\libexec\\x86_64-windows-ghc-9.10.3-b42a\\nonograms-0.1.0.0"
sysconfdir = "C:\\Users\\kelen\\Documents\\GitHub\\Nonograms\\.stack-work\\install\\c51e1627\\etc"

getBinDir     = catchIO (getEnv "nonograms_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "nonograms_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "nonograms_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "nonograms_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "nonograms_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "nonograms_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
