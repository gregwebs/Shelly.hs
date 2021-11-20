{-# OPTIONS -Wall #-}
module Shelly.Directory where

import System.IO.Error (modifyIOError, ioeSetLocation, ioeGetLocation)

import qualified System.PosixCompat as Posix

createFileLink :: String -> String -> IO ()
createFileLink target link =
  (`ioeAddLocation` "createFileLink") `modifyIOError` do
    Posix.createSymbolicLink target link

getSymbolicLinkTarget :: String -> IO String
getSymbolicLinkTarget path =
  (`ioeAddLocation` "getSymbolicLinkTarget") `modifyIOError` do
    Posix.readSymbolicLink path

ioeAddLocation :: IOError -> String -> IOError
ioeAddLocation e loc = do
  ioeSetLocation e newLoc
  where
    newLoc = loc ++ if Prelude.null oldLoc then "" else ":" ++ oldLoc
    oldLoc = ioeGetLocation e
