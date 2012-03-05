{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings,
             MultiParamTypeClasses, FlexibleInstances #-}

-- | A module for shell-like / perl-like programming in Haskell. The stuff in
-- here is not pretty, but it does get job done. The functionality provided by
-- this module is (unlike standard Haskell filesystem functionality)
-- thread-safe: each ShIO maintains its own environment and its own working
-- directory.
module Shelly
       (
         -- * A Text version of FilePath, and convenience functions
         FilePath, liftStringIO, asString
         -- * Entering ShIO.
         , ShIO, shelly, sub, silently, verbosely

         -- * Modifying and querying environment.
         , setenv, getenv, cd, chdir, pwd

         -- * Printing & stuff.
         , echo, echo_n, echo_err, echo_n_err, inspect

         -- * Querying filesystem.
         , ls, test_e, test_f, test_d, test_s, which, find

         -- * Filename helpers
         , dirname, dirnameStr, path, absPath

         -- * Manipulating filesystem.
         , mv, rm_f, rm_rf, cp, cp_r, mkdir, mkdir_p
         , readfile, writefile, appendfile, withTmpDir

         -- * Running external commands.
         , run, (#), run_, command, command_, lastStderr

         -- * Utilities.
         , (</>), (<.>), (<$>), (<$$>), grep, whenM, canonic
         , catch_sh, liftIO, MemTime(..), time, catchany
         , RunFailed(..)
         ) where

import Prelude hiding ( catch, readFile, FilePath )
import Data.List( isInfixOf )
import Data.Char( isAlphaNum )
import Data.Typeable
import Data.IORef
import Data.Maybe
import System.IO hiding ( readFile, FilePath )
import System.PosixCompat.Files( getSymbolicLinkStatus, isSymbolicLink )
import System.Directory
import System.Exit
import System.FilePath hiding (FilePath, (</>), (<.>))
import qualified System.FilePath
import System.Environment
import Control.Applicative
import Control.Exception hiding (handle)
import Control.Monad.Reader
import Control.Concurrent
import Data.Time.Clock( getCurrentTime, diffUTCTime  )

import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.IO as STIO
import System.Process( runInteractiveProcess, waitForProcess, ProcessHandle )

import qualified Data.Text.Lazy as LT
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, fromLazyText, toLazyText)
import Data.Monoid (mappend)

type FilePath = Text

(</>), (<.>) :: FilePath -> FilePath -> FilePath
(</>) d = asString (unpack d System.FilePath.</>)
(<.>) f = asString (unpack f System.FilePath.<.>)

printGetContent :: Handle -> Handle -> IO LT.Text
printGetContent rH wH =
    fmap toLazyText $ printFoldHandleLines (fromText "") foldBuilder rH wH

getContent :: Handle -> IO LT.Text
getContent h = fmap toLazyText $ foldHandleLines (fromText "") foldBuilder h

type FoldCallback a = ((a, LT.Text) -> a)

printFoldHandleLines :: a -> FoldCallback a -> Handle -> Handle -> IO a
printFoldHandleLines start foldLine readHandle writeHandle = go start
  where
    go acc = do
      line <- TIO.hGetLine readHandle
      TIO.hPutStrLn writeHandle line >> (go $ foldLine (acc, line))
     `catchany` \_ -> return acc

foldHandleLines :: a -> FoldCallback a -> Handle -> IO a
foldHandleLines start foldLine readHandle = go start
  where
    go acc = do
      line <- TIO.hGetLine readHandle
      go $ foldLine (acc, line)
     `catchany` \_ -> return acc

data St = St { sCode :: Int
             , sStderr :: LT.Text
             , sDirectory :: FilePath
             , sVerbose :: Bool
             , sRun :: FilePath -> [Text] -> ShIO (Handle, Handle, Handle, ProcessHandle)
             , sEnvironment :: [(String, String)] }

type ShIO a = ReaderT (IORef St) IO a

get :: ShIO St
get = ask >>= liftIO . readIORef

put :: St -> ShIO ()
put v = ask >>= liftIO . flip writeIORef v

modify :: (St -> St) -> ShIO ()
modify f = ask >>= liftIO . flip modifyIORef f

gets :: (St -> a) -> ShIO a
gets f = f <$> get

runInteractiveProcess' :: FilePath -> [Text] -> ShIO (Handle, Handle, Handle, ProcessHandle)
runInteractiveProcess' cmd args = do
  st <- get
  liftIO $ runInteractiveProcess (unpack cmd)
    (map unpack args)
    (Just $ unpack $ sDirectory st)
    (Just $ sEnvironment st)

-- | A helper to catch any exception (same as
-- @... `catch` \(e :: SomeException) -> ...@).
catchany :: IO a -> (SomeException -> IO a) -> IO a
catchany = catch

-- | Catch an exception in the ShIO monad.
catch_sh :: (Exception e) => ShIO a -> (e -> ShIO a) -> ShIO a
catch_sh a h = do ref <- ask
                  liftIO $ catch (runReaderT a ref) (\e -> runReaderT (h e) ref)

-- | Change current working directory of ShIO. This does *not* change the
-- working directory of the process we are running it. Instead, ShIO keeps
-- track of its own workking directory and builds absolute paths internally
-- instead of passing down relative paths. This may have performance
-- repercussions if you are doing hundreds of thousands of filesystem
-- operations. You will want to handle these issues differently in those cases.
cd :: FilePath -> ShIO ()
cd dir = do dir' <- absPath dir
            modify $ \st -> st { sDirectory = dir' }

-- | "cd", execute a ShIO action in the new directory and then pop back to the original directory
chdir :: FilePath -> ShIO a -> ShIO a
chdir dir action = do
  d <- pwd
  cd dir
  r <- action
  cd d
  return r

-- | makes a normalized absolute path
path :: FilePath -> ShIO FilePath
path = absPath >=> return . asString normalise

-- | makes an absolute path. @path@ will also normalize
absPath :: FilePath -> ShIO FilePath
absPath p | isRelative (unpack p) = (</> p) <$> gets sDirectory
           | otherwise = return p
  
-- | apply a String IO operations to a Text FilePath
liftStringIO :: (String -> IO String) -> FilePath -> ShIO FilePath
liftStringIO f = liftIO . f . unpack >=> return . pack

-- | @asString f = pack . f . unpack@
asString :: (String -> String) -> FilePath -> FilePath
asString f = pack . f . unpack

unpack :: FilePath -> String
unpack = LT.unpack

pack :: String -> FilePath
pack = LT.pack

-- | look for unix directory separator '/', don't check for escaping
dirname :: FilePath -> FilePath
dirname = asString dirnameStr

-- | String version of 'dirname'
dirnameStr :: String -> String
dirnameStr = reverse . dropWhile (/= '/') . reverse

-- | Currently a "renameFile" wrapper. TODO: Support cross-filesystem
-- move. TODO: Support directory paths in the second parameter, like in "cp".
mv :: FilePath -> FilePath -> ShIO ()
mv a b = do a' <- absPath a
            b' <- absPath b
            liftIO $ renameFile (unpack a') (unpack b')

-- | List directory contents. Does *not* include \".\" and \"..\", but it does
-- include (other) hidden files.
ls :: FilePath -> ShIO [FilePath]
ls dir = do dir' <- absPath dir
            contents <- liftIO $ getDirectoryContents (unpack dir')
            return [pack c | c <- contents, c `notElem` [".", ".."] ]

-- | List directory recursively (like the POSIX utility "find").
find :: FilePath -> ShIO [FilePath]
find dir = do bits <- ls dir
              subDir <- forM bits $ \x -> do
                ex <- test_d $ dir </> x
                sym <- test_s $ dir </> x
                if ex && not sym then find (dir </> x)
                                 else return []
              return $ map (dir </>) bits ++ concat subDir

-- | Obtain the current (ShIO) working directory.
pwd :: ShIO FilePath
pwd = gets sDirectory

-- | Echo text to standard (error, when using _err variants) output. The _n
-- variants do not print a final newline.
echo, echo_n, echo_err, echo_n_err :: LT.Text -> ShIO ()
echo       = liftIO . TIO.putStrLn
echo_n     = liftIO . (>> hFlush System.IO.stdout) . TIO.putStr
echo_err   = liftIO . TIO.hPutStrLn stderr
echo_n_err = liftIO . (>> hFlush stderr) . TIO.hPutStr stderr

-- | a print lifted into ShIO
inspect :: (Show s) => s -> ShIO ()
inspect = liftIO . print

-- | Create a new directory (fails if the directory exists).
mkdir :: FilePath -> ShIO ()
mkdir = absPath >=> liftIO . createDirectory . unpack

-- | Create a new directory, including parents (succeeds if the directory
-- already exists).
mkdir_p :: FilePath -> ShIO ()
mkdir_p = absPath >=> liftIO . createDirectoryIfMissing True . unpack

-- | Get a full path to an executable on @PATH@, if exists. FIXME does not
-- respect setenv'd environment and uses @PATH@ inherited from the process
-- environment.
which :: FilePath -> ShIO (Maybe FilePath)
which =
  liftIO . findExecutable . unpack >=> return . fmap pack 

-- | Obtain a (reasonably) canonic file path to a filesystem object. Based on
-- "canonicalizePath" in System.FilePath.
canonic :: FilePath -> ShIO FilePath
canonic = absPath >=> liftStringIO canonicalizePath

-- | A monadic-conditional version of the "when" guard.
whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = do res <- c
               when res a

-- | Does a path point to an existing filesystem object?
test_e :: FilePath -> ShIO Bool
test_e f = do fs <- fmap unpack $ absPath f
              liftIO $ do
                dir <- doesDirectoryExist fs
                file <- doesFileExist fs
                return $ file || dir

-- | Does a path point to an existing file?
test_f :: FilePath -> ShIO Bool
test_f = absPath >=> liftIO . doesFileExist . unpack

-- | Does a path point to an existing directory?
test_d :: FilePath -> ShIO Bool
test_d = absPath >=> liftIO . doesDirectoryExist . unpack

-- | Does a path point to a symlink?
test_s :: FilePath -> ShIO Bool
test_s = absPath >=> liftIO . \f -> do
  stat <- getSymbolicLinkStatus (unpack f)
  return $ isSymbolicLink stat

-- | A swiss army cannon for removing things. Actually this goes farther than a
-- normal rm -rf, as it will circumvent permission problems for the files we
-- own. Use carefully.
rm_rf :: FilePath -> ShIO ()
rm_rf f = absPath f >>= \f' -> do
  whenM (test_d f) $ do
    _<- find f' >>= mapM (\file -> liftIO_ $ fixPermissions (unpack file) `catchany` \_ -> return ())
    liftIO_ $ removeDirectoryRecursive (unpack f')
  whenM (test_f f) $ rm_f f'
  where fixPermissions file =
          do permissions <- liftIO $ getPermissions file
             let deletable = permissions { readable = True, writable = True, executable = True }
             liftIO $ setPermissions file deletable

-- | Remove a file. Does not fail if the file already is not there. Does fail
-- if the file is not a file.
rm_f :: FilePath -> ShIO ()
rm_f f = absPath f >>= \f' -> whenM (test_e f) $ liftIO $ removeFile (unpack f')

-- | Set an environment variable. The environment is maintained in ShIO
-- internally, and is passed to any external commands to be executed.
setenv :: String -> String -> ShIO ()
setenv k v = modify $ \x -> x { sEnvironment = wibble $ sEnvironment x }
  where wibble env = (k, v) : filter ((/=k).fst) env

-- | Fetch the current value of an environment variable. Both empty and
-- non-existent variables give empty string as a result.
getenv :: String -> ShIO String
getenv k = fromMaybe "" <$> lookup k <$> gets sEnvironment

-- | Create a sub-ShIO in which external command outputs are not echoed. See "sub".
silently :: ShIO a -> ShIO a
silently a = sub $ modify (\x -> x { sVerbose = False }) >> a

-- | Create a sub-ShIO in which external command outputs are echoed. See "sub".
verbosely :: ShIO a -> ShIO a
verbosely a = sub $ modify (\x -> x { sVerbose = True }) >> a

-- | Enter a sub-ShIO. The new ShIO inherits the environment and working
-- directory from the current one, but the sub-ShIO cannot affect the current
-- one. Exceptions are propagated normally.
sub :: ShIO a -> ShIO a
sub a = do
  st <- get
  r <- a `catch_sh` (\(e :: SomeException) -> put st >> throw e)
  put st
  return r

-- | Enter a ShIO from (Monad)IO. The environment and working directories are
-- inherited from the current process-wide values. Any subsequent changes in
-- processwide working directory or environment are not reflected in the
-- running ShIO.
shelly :: MonadIO m => ShIO a -> m a
shelly a = do
  env <- liftIO $ getEnvironment
  dir <- liftIO $ getCurrentDirectory
  let def   = St { sCode = 0
                 , sStderr = LT.empty
                 , sVerbose = True
                 , sRun = runInteractiveProcess'
                 , sEnvironment = env
                 , sDirectory = pack dir }
  stref <- liftIO $ newIORef def
  liftIO $ runReaderT a stref

data RunFailed = RunFailed String Int LT.Text deriving (Typeable)

instance Show RunFailed where
  show (RunFailed cmd code errs) =
    "error running " ++ cmd ++ ": exit status " ++ show code ++ ":\n" ++ unpack errs

instance Exception RunFailed


-- | An infix shorthand for "run". Write @\"command\" # [ \"argument\" ... ]@.
(#) :: FilePath -> [Text] -> ShIO LT.Text
cmd # args = run cmd args

-- | Execute an external command. Takes the command name (no shell allowed,
-- just a name of something that can be found via @PATH@; FIXME: setenv'd
-- @PATH@ is not taken into account, only the one inherited from the actual
-- outside environment). Nothing is provided on "stdin" of the process, and
-- "stdout" and "stderr" are collected and stored. The "stdout" is returned as
-- a result of "run", and complete stderr output is available after the fact using
-- "lastStderr" 
--
-- All of the stdout output will be loaded into memory
-- You can avoid this but still consume the result by using "run'",
-- or if you need to process the output than "runFoldLines"
run :: FilePath -> [Text] -> ShIO LT.Text
run cmd args = fmap toLazyText $ runFoldLines (fromText "") foldBuilder cmd args

foldBuilder :: (Builder, LT.Text) -> Builder
foldBuilder = (\(b, line) -> b `mappend` fromLazyText line `mappend` singleton '\n')


-- | bind some arguments to run for re-use
-- Example: @monit = command "monit" ["-c", ".monitrc"]@
command :: FilePath -> [Text] -> [Text] -> ShIO LT.Text
command com args more_args = run com (args ++ more_args)

-- | bind some arguments to "run_" for re-use
-- Example: @monit = command' "monit" ["-c", ".monitrc"]@
command_ :: FilePath -> [Text] -> [Text] -> ShIO ()
command_ com args more_args = run_ com (args ++ more_args)

-- the same as "run", but return () instead of the stdout content
run_ :: FilePath -> [Text] -> ShIO ()
run_ cmd args = runFoldLines () (\(_, _) -> ()) cmd args

liftIO_ :: IO a -> ShIO ()
liftIO_ action = liftIO action >> return ()

-- same as "run", but fold over stdout as it is read to avoid keeping it in memory
-- stderr is still placed in memory (this could be changed in the future)
runFoldLines :: a -> FoldCallback a -> FilePath -> [Text] -> ShIO a
runFoldLines start cb cmd args = do
    st <- get
    (_,outH,errH,procH) <- (sRun st) cmd args

    errV <- liftIO newEmptyMVar
    outV <- liftIO newEmptyMVar
    if sVerbose st
      then do
        liftIO_ $ forkIO $ printGetContent errH stderr >>= putMVar errV
        liftIO_ $ forkIO $ printFoldHandleLines start cb outH stdout >>= putMVar outV
      else do
        liftIO_ $ forkIO $ getContent errH >>= putMVar errV
        liftIO_ $ forkIO $ foldHandleLines start cb outH >>= putMVar outV

    errs <- liftIO $ takeMVar errV
    outs <- liftIO $ takeMVar outV
    ex <- liftIO $ waitForProcess procH
    modify $ \x -> x { sStderr = errs }

    case ex of
      ExitSuccess -> do
        modify $ \x -> x { sCode = 0 }
        return ()
      ExitFailure n -> do
        modify $ \x -> x { sCode = n }
        throw $ RunFailed (unpack cmd ++ " " ++ show args) n errs
    return $ outs

-- | The output of last external command. See "run".
lastStderr :: ShIO LT.Text
lastStderr = gets sStderr

data MemTime = MemTime Rational Double deriving (Read, Show, Ord, Eq)

-- | Run a ShIO computation and collect timing (TODO: and memory) information.
time :: ShIO a -> ShIO (MemTime, a)
time what = sub $ do -- TODO track memory usage as well
  t <- liftIO getCurrentTime
  res <- what
  t' <- liftIO getCurrentTime
  let mt = MemTime 0 (realToFrac $ diffUTCTime t' t)
  return (mt, res)

{-
    stats_f <- liftIO $
      do tmpdir <- getTemporaryDirectory
         (f, h) <- openTempFile tmpdir "darcs-stats-XXXX"
         hClose h
         return f
    let args = args' ++ ["+RTS", "-s" ++ stats_f, "-RTS"]
    ...
    stats <- liftIO $ do c <- readFile' stats_f
                         removeFile stats_f `catchany` \e -> hPutStrLn stderr (show e)
                         return c
                       `catchany` \_ -> return ""
    let bytes = (stats =~ "([0-9, ]+) M[bB] total memory in use") :: String
        mem = case length bytes of
          0 -> 0
          _ -> (read (filter (`elem` "0123456789") bytes) :: Int)
    recordMemoryUsed $ mem * 1024 * 1024
    return res
-}

-- | Copy a file, or a directory recursively.
cp_r :: FilePath -> FilePath -> ShIO ()
cp_r from to = do
    whenM (test_d from) $
      mkdir to >> ls from >>= mapM_ (\item -> cp_r (from </> item) (to </> item))
    whenM (test_f from) $ cp from to

-- | Copy a file. The second path could be a directory, in which case the
-- original file name is used, in that directory.
cp :: FilePath -> FilePath -> ShIO ()
cp from to = do
  from' <- absPath from
  to' <- absPath to
  to_dir <- test_d to
  liftIO $ copyFile (unpack from') $ unpack (if to_dir then to' </> asString takeFileName from else to')

class PredicateLike pattern hay where
  match :: pattern -> hay -> Bool

instance PredicateLike (a -> Bool) a where
  match = id

instance (Eq a) => PredicateLike [a] [a] where
  match pat = (pat `isInfixOf`)

-- | Like filter, but more conveniently used with String lists, where a
-- substring match (TODO: also provide regexps, and maybe globs) is expressed as
--  @grep \"needle\" [ \"the\", \"stack\", \"of\", \"hay\" ]@. Boolean
-- predicates just like with "filter" are supported too:
-- @grep (\"fun\" `isPrefixOf`) [...]@.
grep :: (PredicateLike pattern hay) => pattern -> [hay] -> [hay]
grep p l = filter (match p) l

-- | A functor-lifting function composition.
(<$$>) :: (Functor m) => (b -> c) -> (a -> m b) -> a -> m c
f <$$> v = fmap f . v

-- | Create a temporary directory and pass it as a parameter to a ShIO
-- computation. The directory is nuked afterwards.
withTmpDir :: (FilePath -> ShIO a) -> ShIO a
withTmpDir act = do
  dir <- liftIO $ getTemporaryDirectory
  tid <- liftIO $ myThreadId
  (pS, handle) <- liftIO $ openTempFile dir ("tmp"++filter isAlphaNum (show tid))
  let p = pack pS
  liftIO $ hClose handle -- required on windows
  rm_f p
  mkdir p
  a <- act p`catch_sh` \(e :: SomeException) -> rm_rf p >> throw e
  rm_rf p
  return a

-- | Write a Lazy Text to a file.
writefile :: FilePath -> LT.Text -> ShIO ()
writefile f bits = absPath f >>= \f' -> liftIO (TIO.writeFile (unpack f') bits)

-- | Append a Lazy Text to a file.
appendfile :: FilePath -> LT.Text -> ShIO ()
appendfile f bits = absPath f >>= \f' -> liftIO (TIO.appendFile (unpack f') bits)

-- | (Strictly) read file into a Text.
-- All other functions use Lazy Text.
-- So Internally this reads a file as strict text and then converts it to lazy text, which is inefficient
readfile :: FilePath -> ShIO LT.Text
readfile =
  absPath >=> fmap LT.fromStrict . liftIO . STIO.readFile . LT.unpack
