{-# OPTIONS -Wall #-}
module Shelly.Directory where

import System.IO.Error (modifyIOError, ioeSetLocation, ioeGetLocation)
import qualified Filesystem.Path.CurrentOS as FP

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import qualified System.Posix as Posix
#endif

createFileLink :: String -> String -> IO ()
createFileLink target link =
  (`ioeAddLocation` "createFileLink") `modifyIOError` do
#ifdef mingw32_HOST_OS
    Win32.createSymbolicLink False target link
#else
    Posix.createSymbolicLink target link
#endif

getSymbolicLinkTarget :: String -> IO String
getSymbolicLinkTarget path =
  (`ioeAddLocation` "getSymbolicLinkTarget") `modifyIOError` do
#ifdef mingw32_HOST_OS
    Win32.readSymbolicLink path
#else
    Posix.readSymbolicLink path
#endif

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  ioeSetLocation e newLoc
  where
    newLoc = loc ++ if Prelude.null oldLoc then "" else ":" ++ oldLoc
    oldLoc = ioeGetLocation e
