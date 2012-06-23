{-# LANGUAGE OverloadedStrings #-}
-- | prevent circular dependencies
-- needed by multiple exposed modules
module Shelly.Base
  (
    ShIO, State(..), FilePath, Text,
    relPath, path, absPath, canonic, canonicalize,
    test_d, test_s,
    unpack, gets, get, modify, trace,
    ls,
    toTextIgnore,
    echo, echo_n, echo_err, echo_n_err, inspect, inspect_err,
    catchany,
    liftIO, (>=>),
    eitherRelativeTo, relativeTo, maybeRelativeTo
  ) where

import Prelude hiding ( FilePath, catch )
import Data.Text.Lazy (Text)
import Filesystem.Path.CurrentOS (FilePath)
import System.Process( ProcessHandle )
import System.IO ( Handle, hFlush, stderr, stdout )

import Control.Monad.Trans ( liftIO )
import Control.Monad ( (>=>) ) 
import Filesystem (isDirectory, listDirectory)
import System.PosixCompat.Files( getSymbolicLinkStatus, isSymbolicLink )
import Filesystem.Path.CurrentOS (encodeString, relative)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem as FS
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask, ReaderT)
import Data.IORef (readIORef, modifyIORef, IORef)
import Data.Monoid (mappend)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.IO as TIO
import Control.Exception (SomeException, catch)
import Data.Maybe (fromMaybe)

type ShIO a = ReaderT (IORef State) IO a

data State = State   { sCode :: Int
                     , sStdin :: Maybe Text -- ^ stdin for the command to be run
                     , sStderr :: Text
                     , sDirectory :: FilePath
                     , sPrintStdout :: Bool   -- ^ print stdout of command that is executed
                     , sPrintCommands :: Bool -- ^ print command that is executed
                     , sRun :: FilePath -> [Text] -> ShIO (Handle, Handle, Handle, ProcessHandle)
                     , sEnvironment :: [(String, String)]
                     , sTrace :: B.Builder
                     }

-- | Makes a relative path relative to the current ShIO working directory.
-- An absolute path is returned as is.
-- To create an absolute path, use 'absPath'
relPath :: FilePath -> ShIO FilePath
relPath fp = do
  wd  <- gets sDirectory
  rel <- eitherRelativeTo wd fp
  return $ case rel of
    Right p -> p
    Left  p -> p

eitherRelativeTo :: FilePath -- ^ anchor path, the prefix
                 -> FilePath -- ^ make this relative to anchor path
                 -> ShIO (Either FilePath FilePath) -- ^ Left is canonic of second path
eitherRelativeTo relativeFP fp = do
  let fullFp = relativeFP FP.</> fp
  let relDir = addTrailingSlash relativeFP
  stripIt relativeFP fp $ do
    stripIt relativeFP fullFp $ do
      stripIt relDir fp $ do
        stripIt relDir fullFp $ do
          relCan <- canonic relDir
          fpCan  <- canonic fullFp
          stripIt relCan fpCan $ return $ Left fpCan
  where
    stripIt rel toStrip nada = do
      case FP.stripPrefix rel toStrip of
        Just stripped ->
          if stripped == toStrip then nada
            else return $ Right stripped
        Nothing -> nada

-- | make the second path relative to the first
-- Uses 'Filesystem.stripPrefix', but will canonicalize the paths if necessary
relativeTo :: FilePath -- ^ anchor path, the prefix
           -> FilePath -- ^ make this relative to anchor path
           -> ShIO FilePath
relativeTo relativeFP fp =
  fmap (fromMaybe fp) $ maybeRelativeTo relativeFP fp

maybeRelativeTo :: FilePath -- ^ anchor path, the prefix
                 -> FilePath -- ^ make this relative to anchor path
                 -> ShIO (Maybe FilePath)
maybeRelativeTo relativeFP fp = do
  epath <- eitherRelativeTo relativeFP fp
  return $ case epath of
             Right p -> Just p
             Left _ -> Nothing


addTrailingSlash :: FilePath -> FilePath
addTrailingSlash p =
  if FP.null (FP.filename p) then p else
    p FP.</> FP.empty

-- | makes an absolute path.
-- Like 'canonicalize', but on an exception returns 'path'
canonic :: FilePath -> ShIO FilePath
canonic fp = do
  p <- absPath fp
  liftIO $ canonicalizePath p `catchany` \_ -> return p

-- | Obtain a (reasonably) canonic file path to a filesystem object. Based on
-- "canonicalizePath" in system-fileio.
canonicalize :: FilePath -> ShIO FilePath
canonicalize = absPath >=> liftIO . canonicalizePath

-- | bugfix older version of canonicalizePath (system-fileio <= 0.3.7) loses trailing slash
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath p = let was_dir = FP.null (FP.filename p) in
   if not was_dir then FS.canonicalizePath p
     else addTrailingSlash `fmap` FS.canonicalizePath p

-- | Make a relative path absolute by combining with the working directory.
-- An absolute path is returned as is.
-- To create a relative path, use 'path'.
absPath :: FilePath -> ShIO FilePath
absPath p | relative p = (FP.</> p) <$> gets sDirectory
          | otherwise = return p

path :: FilePath -> ShIO FilePath
path = absPath
{-# DEPRECATED path "use absPath, canonic, or relPath instead" #-}

-- | Does a path point to an existing directory?
test_d :: FilePath -> ShIO Bool
test_d = absPath >=> liftIO . isDirectory

-- | Does a path point to a symlink?
test_s :: FilePath -> ShIO Bool
test_s = absPath >=> liftIO . \f -> do
  stat <- getSymbolicLinkStatus (unpack f)
  return $ isSymbolicLink stat

unpack :: FilePath -> String
unpack = encodeString

gets :: (State -> a) -> ShIO a
gets f = f <$> get

get :: ShIO State
get = do
  stateVar <- ask 
  liftIO (readIORef stateVar)

modify :: (State -> State) -> ShIO ()
modify f = do
  state <- ask 
  liftIO (modifyIORef state f)

-- | internally log what occured.
-- Log will be re-played on failure.
trace :: Text -> ShIO ()
trace msg = modify $ \st -> st { sTrace = sTrace st `mappend` B.fromLazyText msg `mappend` "\n" }

-- | List directory contents. Does *not* include \".\" and \"..\", but it does
-- include (other) hidden files.
ls :: FilePath -> ShIO [FilePath]
-- it is important to use path and not absPath so that the listing can remain relative
ls f = absPath f >>= \fp -> do
  trace $ "ls " `mappend` toTextIgnore fp
  filt <- if not (relative f) then return (return)
             else do
               wd <- gets sDirectory
               return (relativeTo wd)
  contents <- liftIO $ listDirectory fp
  mapM filt contents

-- | silently uses the Right or Left value of "Filesystem.Path.CurrentOS.toText"
toTextIgnore :: FilePath -> Text
toTextIgnore fp = LT.fromStrict $ case FP.toText fp of
                                    Left  f -> f
                                    Right f -> f

-- | a print lifted into ShIO
inspect :: (Show s) => s -> ShIO ()
inspect x = do
  (trace . LT.pack . show) x
  liftIO $ print x

-- | a print lifted into ShIO using stderr
inspect_err :: (Show s) => s -> ShIO ()
inspect_err x = do
  let shown = LT.pack $ show x
  trace shown
  echo_err shown

-- | Echo text to standard (error, when using _err variants) output. The _n
-- variants do not print a final newline.
echo, echo_n, echo_err, echo_n_err :: Text -> ShIO ()
echo       = traceLiftIO TIO.putStrLn
echo_n     = traceLiftIO $ (>> hFlush System.IO.stdout) . TIO.putStr
echo_err   = traceLiftIO $ TIO.hPutStrLn stderr
echo_n_err = traceLiftIO $ (>> hFlush stderr) . TIO.hPutStr stderr

traceLiftIO :: (Text -> IO ()) -> Text -> ShIO ()
traceLiftIO f msg = trace ("echo " `mappend` "'" `mappend` msg `mappend` "'") >> liftIO (f msg)

-- | A helper to catch any exception (same as
-- @... `catch` \(e :: SomeException) -> ...@).
catchany :: IO a -> (SomeException -> IO a) -> IO a
catchany = catch

