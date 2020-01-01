{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_typed_lambda_inference (
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

bindir     = "/home/ivanasen/.cabal/bin"
libdir     = "/home/ivanasen/.cabal/lib/x86_64-linux-ghc-8.6.5/typed-lambda-inference-0.1.0.0-B8pWrZ9uStV4aMYa3Hovak"
dynlibdir  = "/home/ivanasen/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/ivanasen/.cabal/share/x86_64-linux-ghc-8.6.5/typed-lambda-inference-0.1.0.0"
libexecdir = "/home/ivanasen/.cabal/libexec/x86_64-linux-ghc-8.6.5/typed-lambda-inference-0.1.0.0"
sysconfdir = "/home/ivanasen/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "typed_lambda_inference_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "typed_lambda_inference_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "typed_lambda_inference_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "typed_lambda_inference_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "typed_lambda_inference_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "typed_lambda_inference_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
