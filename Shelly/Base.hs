{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | I started exposing multiple module (starting with one for finding)
-- Base prevented circular dependencies
-- However, Shelly went back to exposing a single module
module Shelly.Base
  (
    ShIO, Sh, unSh, runSh, State(..), FilePath, Text,
    relPath, path, absPath, canonic, canonicalize,
    test_d, test_s,
    unpack, gets, get, modify, trace,
    ls, lsRelAbs,
    toTextIgnore,
    echo, echo_n, echo_err, echo_n_err, inspect, inspect_err,
    catchany,
    liftIO, (>=>),
    eitherRelativeTo, relativeTo, maybeRelativeTo,
    whenM
    -- * utilities not yet exported
    , addTrailingSlash
  ) where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 760
import Prelude hiding (FilePath, catch)
#else
import Prelude hiding (FilePath)
#endif

import Data.Text (Text)
import System.Process( ProcessHandle )
import System.IO ( Handle, hFlush, stderr, stdout )

import Control.Monad (when, (>=>) )
import Control.Applicative (Applicative, (<$>))
import Filesystem (isDirectory, listDirectory)
import System.PosixCompat.Files( getSymbolicLinkStatus, isSymbolicLink )
import Filesystem.Path.CurrentOS (FilePath, encodeString, relative)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem as FS
import Data.IORef (readIORef, modifyIORef, IORef)
import Data.Monoid (mappend)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (SomeException, catch)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.Reader (MonadReader, runReaderT, ask, ReaderT)

-- | ShIO is Deprecated in favor of 'Sh', which is easier to type.
type ShIO a = Sh a
{- don't need to turn on deprecation. It will cause a lot of warnings while compiling existing code.
 - # DEPRECATED ShIO, "Use Sh instead of ShIO" # -}

newtype Sh a = Sh {
      unSh :: ReaderT (IORef State) IO a
  } deriving (Applicative, Monad, MonadIO, MonadReader (IORef State), Functor)

runSh :: Sh a -> IORef State -> IO a
runSh = runReaderT . unSh

data State = State 
   { sCode :: Int -- ^ exit code for command that ran
   , sStdin :: Maybe Text -- ^ stdin for the command to be run
   , sStderr :: Text -- ^ stderr for command that ran
   , sDirectory :: FilePath -- ^ working directory
   , sPrintStdout :: Bool   -- ^ print stdout of command that is executed
   , sPrintCommands :: Bool -- ^ print command that is executed
   , sRun :: State -> FilePath -> [Text] -> IO (Handle, Handle, Handle, ProcessHandle) -- ^ command runner, a different runner is used when escaping, probably better to just hold the escaping flag
   , sEnvironment :: [(String, String)]
   , sTracing :: Bool -- ^ should we trace command execution
   , sTrace :: Text -- ^ the trace of command execution
   , sErrExit :: Bool -- ^ should we exit immediately on any error
   }

-- | A monadic-conditional version of the "when" guard.
whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= \res -> when res a

-- | Makes a relative path relative to the current Sh working directory.
-- An absolute path is returned as is.
-- To create an absolute path, use 'absPath'
relPath :: FilePath -> Sh FilePath
relPath fp = do
  wd  <- gets sDirectory
  rel <- eitherRelativeTo wd fp
  return $ case rel of
    Right p -> p
    Left  p -> p

eitherRelativeTo :: FilePath -- ^ anchor path, the prefix
                 -> FilePath -- ^ make this relative to anchor path
                 -> Sh (Either FilePath FilePath) -- ^ Left is canonic of second path
eitherRelativeTo relativeFP fp = do
  let fullFp = relativeFP FP.</> fp
  let relDir = addTrailingSlash relativeFP
  stripIt relativeFP fp $
    stripIt relativeFP fullFp $
      stripIt relDir fp $
        stripIt relDir fullFp $ do
          relCan <- canonic relDir
          fpCan  <- canonic fullFp
          stripIt relCan fpCan $ return $ Left fpCan
  where
    stripIt rel toStrip nada =
      case FP.stripPrefix rel toStrip of
        Just stripped ->
          if stripped == toStrip then nada
            else return $ Right stripped
        Nothing -> nada

-- | make the second path relative to the first
-- Uses 'Filesystem.stripPrefix', but will canonicalize the paths if necessary
relativeTo :: FilePath -- ^ anchor path, the prefix
           -> FilePath -- ^ make this relative to anchor path
           -> Sh FilePath
relativeTo relativeFP fp =
  fmap (fromMaybe fp) $ maybeRelativeTo relativeFP fp

maybeRelativeTo :: FilePath -- ^ anchor path, the prefix
                 -> FilePath -- ^ make this relative to anchor path
                 -> Sh (Maybe FilePath)
maybeRelativeTo relativeFP fp = do
  epath <- eitherRelativeTo relativeFP fp
  return $ case epath of
             Right p -> Just p
             Left _ -> Nothing


-- | add a trailing slash to ensure the path indicates a directory
addTrailingSlash :: FilePath -> FilePath
addTrailingSlash p =
  if FP.null (FP.filename p) then p else
    p FP.</> FP.empty

-- | makes an absolute path.
-- Like 'canonicalize', but on an exception returns 'absPath'
canonic :: FilePath -> Sh FilePath
canonic fp = do
  p <- absPath fp
  liftIO $ canonicalizePath p `catchany` \_ -> return p

-- | Obtain a (reasonably) canonic file path to a filesystem object. Based on
-- "canonicalizePath" in system-fileio.
canonicalize :: FilePath -> Sh FilePath
canonicalize = absPath >=> liftIO . canonicalizePath

-- | bugfix older version of canonicalizePath (system-fileio <= 0.3.7) loses trailing slash
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath p = let was_dir = FP.null (FP.filename p) in
   if not was_dir then FS.canonicalizePath p
     else addTrailingSlash `fmap` FS.canonicalizePath p

-- | Make a relative path absolute by combining with the working directory.
-- An absolute path is returned as is.
-- To create a relative path, use 'relPath'.
absPath :: FilePath -> Sh FilePath
absPath p | relative p = (FP.</> p) <$> gets sDirectory
          | otherwise = return p

-- | deprecated
path :: FilePath -> Sh FilePath
path = absPath
{-# DEPRECATED path "use absPath, canonic, or relPath instead" #-}

-- | Does a path point to an existing directory?
test_d :: FilePath -> Sh Bool
test_d = absPath >=> liftIO . isDirectory

-- | Does a path point to a symlink?
test_s :: FilePath -> Sh Bool
test_s = absPath >=> liftIO . \f -> do
  stat <- getSymbolicLinkStatus (unpack f)
  return $ isSymbolicLink stat

unpack :: FilePath -> String
unpack = encodeString

gets :: (State -> a) -> Sh a
gets f = f <$> get

get :: Sh State
get = do
  stateVar <- ask 
  liftIO (readIORef stateVar)

modify :: (State -> State) -> Sh ()
modify f = do
  state <- ask 
  liftIO (modifyIORef state f)

-- | internally log what occurred.
-- Log will be re-played on failure.
trace :: Text -> Sh ()
trace msg =
  whenM (gets sTracing) $ modify $
    \st -> st { sTrace = sTrace st `mappend` msg `mappend` "\n" }

-- | List directory contents. Does *not* include \".\" and \"..\", but it does
-- include (other) hidden files.
ls :: FilePath -> Sh [FilePath]
-- it is important to use path and not absPath so that the listing can remain relative
ls fp = do
  trace $ "ls " `mappend` toTextIgnore fp
  fmap fst $ lsRelAbs fp

lsRelAbs :: FilePath -> Sh ([FilePath], [FilePath])
lsRelAbs f = absPath f >>= \fp -> do
  filt <- if not (relative f) then return return
             else do
               wd <- gets sDirectory
               return (relativeTo wd)
  absolute <- liftIO $ listDirectory fp
  relativized <- mapM filt absolute
  return (relativized, absolute)

-- | silently uses the Right or Left value of "Filesystem.Path.CurrentOS.toText"
toTextIgnore :: FilePath -> Text
toTextIgnore fp = case FP.toText fp of
                    Left  f -> f
                    Right f -> f

-- | a print lifted into 'Sh'
inspect :: (Show s) => s -> Sh ()
inspect x = do
  (trace . T.pack . show) x
  liftIO $ print x

-- | a print lifted into 'Sh' using stderr
inspect_err :: (Show s) => s -> Sh ()
inspect_err x = do
  let shown = T.pack $ show x
  trace shown
  echo_err shown

-- | Echo text to standard (error, when using _err variants) output. The _n
-- variants do not print a final newline.
echo, echo_n, echo_err, echo_n_err :: Text -> Sh ()
echo       = traceLiftIO TIO.putStrLn
echo_n     = traceLiftIO $ (>> hFlush stdout) . TIO.putStr
echo_err   = traceLiftIO $ TIO.hPutStrLn stderr
echo_n_err = traceLiftIO $ (>> hFlush stderr) . TIO.hPutStr stderr

traceLiftIO :: (Text -> IO ()) -> Text -> Sh ()
traceLiftIO f msg = trace ("echo " `mappend` "'" `mappend` msg `mappend` "'") >> liftIO (f msg)

-- | A helper to catch any exception (same as
-- @... `catch` \(e :: SomeException) -> ...@).
catchany :: IO a -> (SomeException -> IO a) -> IO a
catchany = catch

