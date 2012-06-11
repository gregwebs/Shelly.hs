{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings,
             MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, TypeFamilies, IncoherentInstances,
             GADTs
             #-}

-- | A module for shell-like / perl-like programming in Haskell.
-- Shelly's focus is entirely on ease of use for those coming from shell scripting.
-- However, it also tries to use modern libraries and techniques to keep things efficient.
--
-- The functionality provided by
-- this module is (unlike standard Haskell filesystem functionality)
-- thread-safe: each ShIO maintains its own environment and its own working
-- directory.
--
-- I highly recommend putting the following at the top of your program,
-- otherwise you will likely need either type annotations or type conversions
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- > import Data.Text.Lazy as LT
-- > default (LT.Text)
module Shelly
       (
         -- * Entering ShIO.
         ShIO, shelly, sub, silently, verbosely, escaping, print_stdout, print_commands

         -- * Running external commands.
         , run, run_, cmd, (-|-), lastStderr, setStdin
         , command, command_, command1, command1_
         , sshPairs, sshPairs_
 
--         , Sudo(..), run_sudo

         -- * Modifying and querying environment.
         , setenv, getenv, getenv_def, appendToPath

         -- * Environment directory
         , cd, chdir, pwd

         -- * Printing
         , echo, echo_n, echo_err, echo_n_err, inspect, inspect_err
         , tag, trace, show_command

         -- * Querying filesystem.
         , ls, ls', test_e, test_f, test_d, test_s, which,
         -- * Finding files
         find, findWhen, findFold, findDirFilter, findDirFilterWhen, findFoldDirFilter

         -- * Filename helpers
         , path, absPath, (</>), (<.>), relativeTo, canonic

         -- * Manipulating filesystem.
         , mv, rm, rm_f, rm_rf, cp, cp_r, mkdir, mkdir_p
         , readfile, writefile, appendfile, withTmpDir

         -- * Running external commands asynchronously.
         , jobs, background, getBgResult, BgResult

         -- * exiting the program
         , exit, errorExit, terror

         -- * Utilities.
         , (<$>), (<$$>), grep, whenM, unlessM
         , catchany, catch_sh, ShellyHandler(..), catches_sh, catchany_sh
         , Timing(..), time
         , RunFailed(..)

         -- * convert between Text and FilePath
         , toTextIgnore, toTextWarn, fromText

         -- * Re-exported for your convenience
         , liftIO, when, unless, FilePath
         ) where

-- TODO:
-- shebang runner that puts wrappers in and invokes
-- perhaps also adds monadloc
-- convenience for commands that use record arguments
{-
      let oFiles = ("a.o", "b.o")
      let ldOutput x = ("-o", x)

      let def = LD { output = error "", verbose = False, inputs = [] }
      data LD = LD { output :: FilePath, verbose :: Bool, inputs :: [FilePath] } deriving(Data, Typeable)
      instance Runnable LD where
        run :: LD -> IO ()

      class Runnable a where
        run :: a -> ShIO Text

      let ld = def :: LD
      run (ld "foo") { oFiles = [] }
      run ld { oFiles = [] }
      ld = ..magic..
-}

import Prelude hiding ( catch, readFile, FilePath )
import Data.List( isInfixOf )
import Data.Char( isAlphaNum, isSpace )
import Data.Typeable
import Data.IORef
import Data.Maybe
import System.IO hiding ( readFile, FilePath )
import System.Exit
import System.Environment
import Control.Applicative
import Control.Exception hiding (handle)
import Control.Monad.Reader
import Control.Concurrent
import qualified Control.Concurrent.MSem as Sem
import Data.Time.Clock( getCurrentTime, diffUTCTime  )

import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.IO as STIO
import System.Process( CmdSpec(..), StdStream(CreatePipe), CreateProcess(..), createProcess, waitForProcess, ProcessHandle )

import qualified Data.Text.Lazy as LT
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text as T
import Data.Monoid (mappend)

import Filesystem.Path.CurrentOS hiding (concat, fromText, (</>), (<.>))
import Filesystem hiding (canonicalizePath)
import qualified Filesystem
import qualified Filesystem.Path.CurrentOS as FP

import System.PosixCompat.Files( getSymbolicLinkStatus, isSymbolicLink )
import System.Directory ( setPermissions, getPermissions, Permissions(..), getTemporaryDirectory, findExecutable ) 

{- GHC won't default to Text with this, even with extensions!
 - see: http://hackage.haskell.org/trac/ghc/ticket/6030
class ShellArgs a where
  toTextArgs :: a -> [Text]

instance ShellArgs Text       where toTextArgs t = [t]
instance ShellArgs FilePath   where toTextArgs t = [toTextIgnore t]
instance ShellArgs [Text]     where toTextArgs = id
instance ShellArgs [FilePath] where toTextArgs = map toTextIgnore

instance ShellArgs (Text, Text) where
  toTextArgs (t1,t2) = [t1, t2]
instance ShellArgs (FilePath, FilePath) where
  toTextArgs (fp1,fp2) = [toTextIgnore fp1, toTextIgnore fp2]
instance ShellArgs (Text, FilePath) where
  toTextArgs (t1, fp1) = [t1, toTextIgnore fp1]
instance ShellArgs (FilePath, Text) where
  toTextArgs (fp1,t1) = [toTextIgnore fp1, t1]

cmd :: (ShellArgs args) => FilePath -> args -> ShIO Text
cmd fp args = run fp $ toTextArgs args
-}

-- | Converter for the variadic argument version of 'run' called 'cmd'.
class ShellArg a where toTextArg :: a -> Text
instance ShellArg Text     where toTextArg = id
instance ShellArg FilePath where toTextArg = toTextIgnore


-- Voodoo to create the variadic function 'cmd'
class ShellCommand t where
    cmdAll :: FilePath -> [Text] -> t

instance ShellCommand (ShIO Text) where
    cmdAll fp args = run fp args

instance (s ~ Text, Show s) => ShellCommand (ShIO s) where
    cmdAll fp args = run fp args

-- note that ShIO () actually doesn't work for its case (_<- cmd) when there is no type signature
instance ShellCommand (ShIO ()) where
    cmdAll fp args = run_ fp args >> liftIO (throwIO CmdError)

data CmdError = CmdError deriving Typeable
instance Show CmdError where
  show (CmdError) = "Sorry! You are running up against some of the magic from using the variadic argument function 'cmd'. Please report this issue so we can fix it."

instance Exception CmdError

instance (ShellArg arg, ShellCommand result) => ShellCommand (arg -> result) where
    cmdAll fp acc = \x -> cmdAll fp (acc ++ [toTextArg x])

-- | variadic argument version of run.
-- The syntax is more convenient but it also allows the use of a FilePath as a command argument.
-- So an argument can be a Text or a FilePath.
-- a FilePath is converted to Text with 'toTextIgnore'.
-- You will need to add the following to your module:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- > import Shelly
-- > import Data.Text.Lazy as LT
-- > default (LT.Text)
--
cmd :: (ShellCommand result) => FilePath -> result
cmd fp = cmdAll fp []

-- | Helper to convert a Text to a FilePath. Used by '(</>)' and '(<.>)'
class ToFilePath a where
  toFilePath :: a -> FilePath

instance ToFilePath FilePath where toFilePath = id
instance ToFilePath Text     where toFilePath = fromText
instance ToFilePath T.Text   where toFilePath = FP.fromText
instance ToFilePath String   where toFilePath = FP.fromText . T.pack


-- | uses System.FilePath.CurrentOS, but can automatically convert a Text
(</>) :: (ToFilePath filepath1, ToFilePath filepath2) => filepath1 -> filepath2 -> FilePath
x </> y = toFilePath x FP.</> toFilePath y

-- | uses System.FilePath.CurrentOS, but can automatically convert a Text
(<.>) :: (ToFilePath filepath) => filepath -> Text -> FilePath
x <.> y = toFilePath x FP.<.> LT.toStrict y


-- | silently uses the Right or Left value of "Filesystem.Path.CurrentOS.toText"
toTextIgnore :: FilePath -> Text
toTextIgnore fp = LT.fromStrict $ case toText fp of
                                    Left  f -> f
                                    Right f -> f

toTextWarn :: FilePath -> ShIO Text
toTextWarn efile = fmap lazy $ case toText efile of
    Left f -> encodeError f >> return f
    Right f -> return f
  where
    encodeError f = echo ("Invalid encoding for file: " `mappend` lazy f)
    lazy = LT.fromStrict

fromText :: Text -> FilePath
fromText = FP.fromText . LT.toStrict

printGetContent :: Handle -> Handle -> IO Text
printGetContent rH wH =
    fmap B.toLazyText $ printFoldHandleLines (B.fromText "") foldBuilder rH wH

getContent :: Handle -> IO Text
getContent h = fmap B.toLazyText $ foldHandleLines (B.fromText "") foldBuilder h

type FoldCallback a = ((a, Text) -> a)

printFoldHandleLines :: a -> FoldCallback a -> Handle -> Handle -> IO a
printFoldHandleLines start foldLine readHandle writeHandle = go start
  where
    go acc = do
      line <- TIO.hGetLine readHandle
      TIO.hPutStrLn writeHandle line >> go (foldLine (acc, line))
     `catchany` \_ -> return acc

foldHandleLines :: a -> FoldCallback a -> Handle -> IO a
foldHandleLines start foldLine readHandle = go start
  where
    go acc = do
      line <- TIO.hGetLine readHandle
      go $ foldLine (acc, line)
     `catchany` \_ -> return acc

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

-- | same as 'trace', but use it combinator style
tag :: ShIO a -> Text -> ShIO a
tag action msg = do
  trace msg
  result <- action
  return result


-- | log actions that occur
trace :: Text -> ShIO ()
trace msg = modify $ \st -> st { sTrace = sTrace st `mappend` B.fromLazyText msg `mappend` "\n" }

type ShIO a = ReaderT (IORef State) IO a

get :: ShIO State
get = do
  stateVar <- ask 
  liftIO (readIORef stateVar)

put :: State -> ShIO ()
put newState = do
  stateVar <- ask 
  liftIO (writeIORef stateVar newState)

modify :: (State -> State) -> ShIO ()
modify f = do
  state <- ask 
  liftIO (modifyIORef state f)


gets :: (State -> a) -> ShIO a
gets f = f <$> get

-- FIXME: find the full path to the exe from PATH
runCommand :: FilePath -> [Text] -> ShIO (Handle, Handle, Handle, ProcessHandle)
runCommand exe args = do
  st <- get
  shellyProcess st $
    RawCommand (unpack exe) (map LT.unpack args)

runCommandNoEscape :: FilePath -> [Text] -> ShIO (Handle, Handle, Handle, ProcessHandle)
runCommandNoEscape exe args = do
  st <- get
  shellyProcess st $
    ShellCommand $ LT.unpack $ LT.intercalate " " (toTextIgnore exe : args)
    

shellyProcess :: State -> CmdSpec -> ShIO (Handle, Handle, Handle, ProcessHandle)
shellyProcess st cmdSpec =  do
  (Just hin, Just hout, Just herr, pHandle) <- liftIO $
    createProcess $ CreateProcess {
        cmdspec = cmdSpec
      , cwd = Just $ unpack $ sDirectory st
      , env = Just $ sEnvironment st
      , std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
      , close_fds = False
#if MIN_VERSION_process(1,1,0)
      , create_group = False
#endif
      }
  return (hin, hout, herr, pHandle)

{-
-- | use for commands requiring usage of sudo. see 'run_sudo'.
--  Use this pattern for priveledge separation
newtype Sudo a = Sudo { sudo :: ShIO a }

-- | require that the caller explicitly state 'sudo'
run_sudo :: Text -> [Text] -> Sudo Text
run_sudo cmd args = Sudo $ run "/usr/bin/sudo" (cmd:args)
-}

-- | A helper to catch any exception (same as
-- @... `catch` \(e :: SomeException) -> ...@).
catchany :: IO a -> (SomeException -> IO a) -> IO a
catchany = catch

-- | Catch an exception in the ShIO monad.
catch_sh :: (Exception e) => ShIO a -> (e -> ShIO a) -> ShIO a
catch_sh action handle = do
    ref <- ask
    liftIO $ catch (runReaderT action ref) (\e -> runReaderT (handle e) ref)


-- | You need this when using 'catches_sh'.
data ShellyHandler a = forall e . Exception e => ShellyHandler (e -> ShIO a)

-- | Catch multiple exceptions in the ShIO monad.
catches_sh :: ShIO a -> [ShellyHandler a] -> ShIO a
catches_sh action handlers = do
    ref <- ask
    let runner a = runReaderT a ref
    liftIO $ catches (runner action) $ map (toHandler runner) handlers
  where
    toHandler :: (ShIO a -> IO a) -> ShellyHandler a -> Handler a
    toHandler runner (ShellyHandler handle) = Handler (\e -> runner (handle e))

-- | Catch an exception in the ShIO monad.
catchany_sh :: ShIO a -> (SomeException -> ShIO a) -> ShIO a
catchany_sh = catch_sh

-- | Change current working directory of ShIO. This does *not* change the
-- working directory of the process we are running it. Instead, ShIO keeps
-- track of its own workking directory and builds absolute paths internally
-- instead of passing down relative paths. This may have performance
-- repercussions if you are doing hundreds of thousands of filesystem
-- operations. You will want to handle these issues differently in those cases.
cd :: FilePath -> ShIO ()
cd dir = do dir' <- absPath dir
            trace $ "cd " `mappend` toTextIgnore dir'
            modify $ \st -> st { sDirectory = dir' }

-- | "cd", execute a ShIO action in the new directory and then pop back to the original directory
chdir :: FilePath -> ShIO a -> ShIO a
chdir dir action = do
  d <- pwd
  cd dir
  r <- action `catchany_sh` (\e ->
      cd d >> liftIO (throwIO e)
    )
  cd d
  return r

-- | makes an absolute path.
-- Like 'canonic', but on an exception returns 'absPath'
path :: FilePath -> ShIO FilePath
path fp = do
  absFP <- absPath fp
  liftIO $ canonicalizePath absFP `catchany` \_ -> return absFP

-- | makes an absolute path based on the working directory.
-- @path@ will also canonicalize
absPath :: FilePath -> ShIO FilePath
absPath p | relative p = (FP.</> p) <$> gets sDirectory
          | otherwise = return p
  
-- | apply a String IO operations to a Text FilePath
{-
liftStringIO :: (String -> IO String) -> FilePath -> ShIO FilePath
liftStringIO f = liftIO . f . unpack >=> return . pack

-- | @asString f = pack . f . unpack@
asString :: (String -> String) -> FilePath -> FilePath
asString f = pack . f . unpack
-}

unpack :: FilePath -> String
unpack = encodeString

pack :: String -> FilePath
pack = decodeString

-- | Currently a "renameFile" wrapper. TODO: Support cross-filesystem
-- move. TODO: Support directory paths in the second parameter, like in "cp".
mv :: FilePath -> FilePath -> ShIO ()
mv a b = do a' <- absPath a
            b' <- absPath b
            trace $ "mv " `mappend` toTextIgnore a' `mappend` " " `mappend` toTextIgnore b'
            liftIO $ rename a' b'

-- | Get back [Text] instead of [FilePath]
ls' :: FilePath -> ShIO [Text]
ls' fp = ls fp >>= mapM toTextWarn

-- | List directory contents. Does *not* include \".\" and \"..\", but it does
-- include (other) hidden files.
ls :: FilePath -> ShIO [FilePath]
ls fp = (liftIO $ listDirectory fp) `tag` ("ls " `mappend` toTextIgnore fp)

-- | make the second path relative to the first
-- Uses 'Filesystem.stripPrefix', but will canonicalize the paths if necessary
relativeTo :: FilePath -- ^ anchor path, the prefix
           -> FilePath -- ^ make this relative to anchor path
           -> ShIO FilePath
relativeTo relativeFP fp = do
  stripIt relativeFP fp $ do
    isDir <- test_d relativeFP
    let relDir = if not isDir then relativeFP else addTrailingSlash relativeFP
    relAbs <- path relDir
    absFP  <- path fp
    stripIt relAbs absFP $ return fp
  where
    stripIt rel toStrip nada = do
      case stripPrefix rel toStrip of
        Just stripped ->
          if stripped == toStrip then nada
            else return stripped
        Nothing -> nada

addTrailingSlash :: FilePath -> FilePath
addTrailingSlash p =
  if FP.null (filename p) then p else
    p FP.</> FP.empty

-- | bugfix older version of canonicalizePath (system-fileio <= 0.3.7) loses trailing slash
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath p = let was_dir = FP.null (filename p) in
   if not was_dir then Filesystem.canonicalizePath p
     else addTrailingSlash `fmap` Filesystem.canonicalizePath p


-- | List directory recursively (like the POSIX utility "find").
-- listing is relative if the path given is relative.
-- If you want to filter out some results or fold over them you can do that with the returned files.
-- A more efficient approach is to use one of the other find functions.
find :: FilePath -> ShIO [FilePath]
find dir = findFold dir [] (\paths fp -> return $ paths ++ [fp])

-- | 'find' that filters the found files as it finds.
-- Files must satisfy the given filter to be returned in the result.
findWhen :: FilePath -> (FilePath -> ShIO Bool) -> ShIO [FilePath]
findWhen dir filt = findDirFilterWhen dir (const $ return True) filt

-- | Fold an arbitrary folding function over files froma a 'find'.
-- Like 'findWhen' but use a more general fold rather than a filter.
findFold :: FilePath -> a -> (a -> FilePath -> ShIO a) -> ShIO a
findFold fp = findFoldDirFilter fp (const $ return True)

-- | 'find' that filters out directories as it finds
-- Filtering out directories can make a find much more efficient by avoiding entire trees of files.
findDirFilter :: FilePath -> (FilePath -> ShIO Bool) -> ShIO [FilePath]
findDirFilter dir filt = findDirFilterWhen dir filt (const $ return True)

-- | similar 'findWhen', but also filter out directories
-- Alternatively, similar to 'findDirFilter', but also filter out files
-- Filtering out directories makes the find much more efficient
findDirFilterWhen :: FilePath -- ^ directory
                  -> (FilePath -> ShIO Bool) -- ^ directory filter
                  -> (FilePath -> ShIO Bool) -- ^ file filter
                  -> ShIO [FilePath]
findDirFilterWhen dir dirFilt fileFilter = findFoldDirFilter dir dirFilt [] filterIt
  where
    filterIt paths fp = do
      yes <- fileFilter fp
      return $ if yes then paths ++ [fp] else paths

-- | like 'findDirFilterWhen' but use a folding function rather than a filter
-- The most general finder: you likely want a more specific one
findFoldDirFilter :: FilePath -> (FilePath -> ShIO Bool) -> a -> (a -> FilePath -> ShIO a) -> ShIO a
findFoldDirFilter dir dirFilter startValue folder = do
  trace ("findFold " `mappend` toTextIgnore dir)
  filt <- dirFilter dir
  if filt
    then ls dir >>= foldM traverse startValue
    else return startValue
  where
    traverse acc x = do
      isDir <- test_d x
      sym <- test_s x
      if isDir && not sym
        then findFold x acc folder
        else folder acc x

-- | Obtain the current (ShIO) working directory.
pwd :: ShIO FilePath
pwd = gets sDirectory `tag` "pwd"

-- | Echo text to standard (error, when using _err variants) output. The _n
-- variants do not print a final newline.
echo, echo_n, echo_err, echo_n_err :: Text -> ShIO ()
echo       = traceLiftIO TIO.putStrLn
echo_n     = traceLiftIO $ (>> hFlush System.IO.stdout) . TIO.putStr
echo_err   = traceLiftIO $ TIO.hPutStrLn stderr
echo_n_err = traceLiftIO $ (>> hFlush stderr) . TIO.hPutStr stderr

traceLiftIO :: (Text -> IO ()) -> Text -> ShIO ()
traceLiftIO f msg = trace ("echo " `mappend` "'" `mappend` msg `mappend` "'") >> liftIO (f msg)

exit :: Int -> ShIO ()
exit 0 = liftIO (exitWith ExitSuccess) `tag` "exit 0"
exit n = liftIO (exitWith (ExitFailure n)) `tag` ("exit " `mappend` LT.pack (show n))

errorExit :: Text -> ShIO ()
errorExit msg = echo msg >> exit 1

-- | fail that takes a Text
terror :: Text -> ShIO a
terror = fail . LT.unpack

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

-- | Create a new directory (fails if the directory exists).
mkdir :: FilePath -> ShIO ()
mkdir = absPath >=> \fp -> do
  trace $ "mkdir " `mappend` toTextIgnore fp
  liftIO $ createDirectory False fp `catchany` (\e -> throwIO e >> return ())

-- | Create a new directory, including parents (succeeds if the directory
-- already exists).
mkdir_p :: FilePath -> ShIO ()
mkdir_p = absPath >=> \fp -> do
  trace $ "mkdir -p " `mappend` toTextIgnore fp
  liftIO $ createTree fp

-- | Get a full path to an executable on @PATH@, if exists. FIXME does not
-- respect setenv'd environment and uses @findExecutable@ which uses the @PATH@ inherited from the process
-- environment.
-- FIXME: findExecutable does not maintain a hash of existing commands and does a ton of file stats
which :: FilePath -> ShIO (Maybe FilePath)
which fp = do
  (trace . mappend "which " . toTextIgnore) fp
  (liftIO . findExecutable . unpack >=> return . fmap pack) fp

-- | Obtain a (reasonably) canonic file path to a filesystem object. Based on
-- "canonicalizePath" in FileSystem.
canonic :: FilePath -> ShIO FilePath
canonic = absPath >=> liftIO . canonicalizePath

-- | A monadic-conditional version of the "when" guard.
whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= \res -> when res a

-- | A monadic-conditional version of the "unless" guard.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= \res -> unless res a

-- | Does a path point to an existing filesystem object?
test_e :: FilePath -> ShIO Bool
test_e f = do
  fs <- absPath f
  liftIO $ do
    file <- isFile fs
    if file then return True else isDirectory fs

-- | Does a path point to an existing file?
test_f :: FilePath -> ShIO Bool
test_f = absPath >=> liftIO . isFile

-- | Does a path point to an existing directory?
test_d :: FilePath -> ShIO Bool
test_d = absPath >=> liftIO . isDirectory

-- | Does a path point to a symlink?
test_s :: FilePath -> ShIO Bool
test_s = absPath >=> liftIO . \f -> do
  stat <- getSymbolicLinkStatus (unpack f)
  return $ isSymbolicLink stat

-- | A swiss army cannon for removing things. Actually this goes farther than a
-- normal rm -rf, as it will circumvent permission problems for the files we
-- own. Use carefully.
-- Uses 'removeTree'
rm_rf :: FilePath -> ShIO ()
rm_rf f = absPath f >>= \f' -> do
  trace $ "rm -rf " `mappend` toTextIgnore f
  whenM (test_d f) $ do
    _<- find f' >>= mapM (\file -> liftIO_ $ fixPermissions (unpack file) `catchany` \_ -> return ())
    liftIO_ $ removeTree f'
  whenM (test_f f) $ rm_f f'
  where fixPermissions file =
          do permissions <- liftIO $ getPermissions file
             let deletable = permissions { readable = True, writable = True, executable = True }
             liftIO $ setPermissions file deletable

-- | Remove a file. Does not fail if the file already is not there.
-- Does fail if the file is not a file.
rm_f :: FilePath -> ShIO ()
rm_f f = do
  trace $ "rm -f " `mappend` toTextIgnore f
  whenM (test_e f) $ absPath f >>= liftIO . removeFile

-- | Remove a file.
-- Does fail if the file does not exist (use 'rm_f' instead) or is not a file.
rm :: FilePath -> ShIO ()
rm f = do
  trace $ "rm" `mappend` toTextIgnore f
  absPath f >>= liftIO . removeFile

-- | Set an environment variable. The environment is maintained in ShIO
-- internally, and is passed to any external commands to be executed.
setenv :: Text -> Text -> ShIO ()
setenv k v =
  let (kStr, vStr) = (LT.unpack k, LT.unpack v)
      wibble environment = (kStr, vStr) : filter ((/=kStr).fst) environment
   in modify $ \x -> x { sEnvironment = wibble $ sEnvironment x }

-- | add the filepath onto the PATH env variable
-- FIXME: only effects the PATH once the process is ran, as per comments in 'which'
appendToPath :: FilePath -> ShIO ()
appendToPath filepath = do
  tp <- toTextWarn filepath
  pe <- getenv path_env
  setenv path_env $ pe `mappend` ":" `mappend` tp
  where
    path_env = "PATH"

-- | Fetch the current value of an environment variable. Both empty and
-- non-existent variables give empty string as a result.
getenv :: Text -> ShIO Text
getenv k = getenv_def k ""

-- | Fetch the current value of an environment variable. Both empty and
-- non-existent variables give the default value as a result
getenv_def :: Text -> Text -> ShIO Text
getenv_def k d = gets sEnvironment >>=
  return . LT.pack . fromMaybe (LT.unpack d) . lookup (LT.unpack k)

-- | Create a sub-ShIO in which external command outputs are not echoed.
-- Also commands are not printed.
-- See "sub".
silently :: ShIO a -> ShIO a
silently a = sub $ modify (\x -> x { sPrintStdout = False, sPrintCommands = False }) >> a

-- | Create a sub-ShIO in which external command outputs are echoed.
-- Executed commands are printed
-- See "sub".
verbosely :: ShIO a -> ShIO a
verbosely a = sub $ modify (\x -> x { sPrintStdout = True, sPrintCommands = True }) >> a

-- | Turn on/off printing stdout
print_stdout :: Bool -> ShIO a -> ShIO a
print_stdout shouldPrint a = sub $ modify (\x -> x { sPrintStdout = shouldPrint }) >> a

-- | Create a 'BgJobManager' that has a 'limit' on the max number of background tasks.
-- an invocation of jobs is independent of any others, and not tied to the ShIO monad in any way.
-- This blocks the execution of the program until all 'background' jobs are finished.
jobs :: Int -> (BgJobManager -> ShIO a) -> ShIO a
jobs limit action = do
    unless (limit > 0) $ terror "expected limit to be > 0"
    availableJobsSem <- liftIO $ Sem.new limit
    res <- action $ BgJobManager availableJobsSem
    liftIO $ waitForJobs availableJobsSem
    return res
  where
    waitForJobs sem = do
      avail <- Sem.peekAvail sem
      if avail == limit then return () else waitForJobs sem

-- | The manager tracks the number of jobs. Register your 'background' jobs with it.
newtype BgJobManager = BgJobManager (Sem.MSem Int)

-- | Type returned by tasks run asynchronously in the background.
newtype BgResult a = BgResult (MVar a)

-- | Returns the promised result from a backgrounded task.  Blocks until
-- the task completes.
getBgResult :: BgResult a -> ShIO a
getBgResult (BgResult mvar) = liftIO $ takeMVar mvar

-- | Run the `ShIO` task asynchronously in the background, returns
-- the `BgResult a`, a promise immediately. Run "getBgResult" to wait for the result.
-- The background task will inherit the current ShIO context
-- The 'BjJobManager' ensures the max jobs limit must be sufficient for the parent and all children.
background :: BgJobManager -> ShIO a -> ShIO (BgResult a)
background (BgJobManager manager) proc = do
  state <- get
  liftIO $ do
    -- take up a spot
    -- It is important to do this before forkIO:
    -- It ensures that that jobs will block and the program won't exit before our jobs are done
    -- On the other hand, a user might not expect 'jobs' to block
    Sem.wait manager
    mvar <- newEmptyMVar -- future result

    _<- forkIO $ do
      result <- shelly $ (put state >> proc)
      Sem.signal manager -- open a spot back up
      liftIO $ putMVar mvar result
    return $ BgResult mvar


-- | Turn on/off command echoing.
print_commands :: Bool -> ShIO a -> ShIO a
print_commands shouldPrint a = sub $ modify (\st -> st { sPrintCommands = shouldPrint }) >> a

-- | Enter a sub-ShIO that inherits the environment
-- The original state will be restored when the sub-ShIO completes.
-- Exceptions are propagated normally.
sub :: ShIO a -> ShIO a
sub a = do
  oldState <- get
  modify $ \st -> st { sTrace = B.fromText "" }
  r <- a `catchany_sh` (\e -> do
	restoreState oldState
	liftIO $ throwIO e)
  restoreState oldState
  return r
  where
    restoreState oldState = do
      newState <- get
      put oldState { sTrace = sTrace oldState `mappend` sTrace newState  }

escaping :: Bool -> ShIO a -> ShIO a
escaping shouldEscape action = sub $ do
  modify $ \st -> st { sRun =
      if shouldEscape
        then runCommand
        else runCommandNoEscape
    }
  action

-- | Enter a ShIO from (Monad)IO. The environment and working directories are
-- inherited from the current process-wide values. Any subsequent changes in
-- processwide working directory or environment are not reflected in the
-- running ShIO.
shelly :: MonadIO m => ShIO a -> m a
shelly action = do
  environment <- liftIO getEnvironment
  dir <- liftIO getWorkingDirectory
  let def  = State { sCode = 0
                   , sStdin = Nothing
                   , sStderr = LT.empty
                   , sPrintStdout = True
                   , sPrintCommands = False
                   , sRun = runCommand
                   , sEnvironment = environment
                   , sTrace = B.fromText ""
                   , sDirectory = dir }
  stref <- liftIO $ newIORef def
  let caught =
        action `catches_sh` [
              ShellyHandler (\ex ->
                case ex of
                  ExitSuccess   -> liftIO $ throwIO ex
                  ExitFailure _ -> throwExplainedException ex
              )
            , ShellyHandler (\(ex::SomeException) -> throwExplainedException ex)
          ]
  liftIO $ runReaderT caught stref
  where
    throwExplainedException :: Exception exception => exception -> ShIO a
    throwExplainedException ex = get >>=
      liftIO . throwIO . ReThrownException ex . errorMsg . LT.unpack .  B.toLazyText . sTrace
    errorMsg trc = "Ran commands: \n" `mappend` trc

data RunFailed = RunFailed FilePath [Text] Int Text deriving (Typeable)

instance Show RunFailed where
  show (RunFailed exe args code errs) =
    let codeMsg = case code of
          127 -> ". exit code 127 usually means the command does not exist (in the PATH)"
          _ -> ""
    in "error running: " ++ LT.unpack (show_command exe args) ++
         "\nexit status: " ++ show code ++ codeMsg ++ "\nstderr: " ++ LT.unpack errs

instance Exception RunFailed

show_command :: FilePath -> [Text] -> Text
show_command exe args =
    LT.intercalate " " $ map quote (toTextIgnore exe : args)
  where
    quote t = if LT.any (== '\'') t then t
      else if LT.any isSpace t then surround '\'' t else t

surround :: Char -> Text -> Text
surround c t = LT.cons c $ LT.snoc t c

-- | same as 'sshPairs', but returns ()
sshPairs_ :: Text -> [(FilePath, [Text])] -> ShIO ()
sshPairs_ _ [] = return ()
sshPairs_ server cmds = sshPairs' run_ server cmds

-- | run commands over SSH.
-- An ssh executable is expected in your path.
-- Commands are in the same form as 'run', but given as pairs
--
-- > sshPairs "server-name" [("cd", "dir"), ("rm",["-r","dir2"])]
--
-- I am not fond of this interface, but it seems to work.
--
-- Please note this sets 'escaping' to False: the commands will not be shell escaped.
-- I think this should be more convenient for ssh.
-- Internally the list of commands are combined with the string " && " before given to ssh.
sshPairs :: Text -> [(FilePath, [Text])] -> ShIO Text
sshPairs _ [] = return ""
sshPairs server cmds = sshPairs' run server cmds

sshPairs' :: (FilePath -> [Text] -> ShIO a) -> Text -> [(FilePath, [Text])] -> ShIO a
sshPairs' run' server actions = do
  escaping False $ do
    let ssh_commands = surround '\'' $ foldl1
          (\memo next -> memo `mappend` " && " `mappend` next)
          (map toSSH actions)
    run' "ssh" $ [server, ssh_commands]
  where
    toSSH (exe,args) = show_command exe args


data Exception e => ReThrownException e = ReThrownException e String deriving (Typeable)
instance Exception e => Exception (ReThrownException e)
instance Exception e => Show (ReThrownException e) where
  show (ReThrownException ex msg) = "\n" ++
    msg ++ "\n" ++ "Exception: " ++ show ex

-- | Execute an external command. Takes the command name (no shell allowed,
-- just a name of something that can be found via @PATH@; FIXME: setenv'd
-- @PATH@ is not taken into account when finding the exe name)
--
-- "stdout" and "stderr" are collected. The "stdout" is returned as
-- a result of "run", and complete stderr output is available after the fact using
-- "lastStderr" 
--
-- All of the stdout output will be loaded into memory
-- You can avoid this but still consume the result by using "run_",
-- If you want to avoid the memory and need to process the output then use "runFoldLines".
run :: FilePath -> [Text] -> ShIO Text
run exe args = fmap B.toLazyText $ runFoldLines (B.fromText "") foldBuilder exe args

foldBuilder :: (B.Builder, Text) -> B.Builder
foldBuilder (b, line) = b `mappend` B.fromLazyText line `mappend` B.singleton '\n'


-- | bind some arguments to run for re-use
-- Example: @monit = command "monit" ["-c", "monitrc"]@
command :: FilePath -> [Text] -> [Text] -> ShIO Text
command com args more_args = run com (args ++ more_args)

-- | bind some arguments to "run_" for re-use
-- Example: @monit_ = command_ "monit" ["-c", "monitrc"]@
command_ :: FilePath -> [Text] -> [Text] -> ShIO ()
command_ com args more_args = run_ com (args ++ more_args)

-- | bind some arguments to run for re-use, and expect 1 argument
-- Example: @git = command1 "git" []; git "pull" ["origin", "master"]@
command1 :: FilePath -> [Text] -> Text -> [Text] -> ShIO Text
command1 com args one_arg more_args = run com ([one_arg] ++ args ++ more_args)

-- | bind some arguments to run for re-use, and expect 1 argument
-- Example: @git_ = command1_ "git" []; git+ "pull" ["origin", "master"]@
command1_ :: FilePath -> [Text] -> Text -> [Text] -> ShIO ()
command1_ com args one_arg more_args = run_ com ([one_arg] ++ args ++ more_args)

-- the same as "run", but return () instead of the stdout content
run_ :: FilePath -> [Text] -> ShIO ()
run_ = runFoldLines () (\(_, _) -> ())

liftIO_ :: IO a -> ShIO ()
liftIO_ action = liftIO action >> return ()

-- same as "run", but fold over stdout as it is read to avoid keeping it in memory
-- stderr is still placed in memory (this could be changed in the future)
runFoldLines :: a -> FoldCallback a -> FilePath -> [Text] -> ShIO a
runFoldLines start cb exe args = do
    -- clear stdin before beginning command execution
    origstate <- get
    let mStdin = sStdin origstate
    put $ origstate { sStdin = Nothing, sCode = 0, sStderr = LT.empty }
    state <- get

    let cmdString = show_command exe args
    when (sPrintCommands state) $ echo cmdString
    trace cmdString

    (inH,outH,errH,procH) <- sRun state exe args

    case mStdin of
      Just input ->
        liftIO $ TIO.hPutStr inH input >> hClose inH
        -- stdin is cleared from state below
      Nothing -> return ()

    errV <- liftIO newEmptyMVar
    outV <- liftIO newEmptyMVar
    if sPrintStdout state
      then do
        liftIO_ $ forkIO $ printGetContent errH stderr >>= putMVar errV
        liftIO_ $ forkIO $ printFoldHandleLines start cb outH stdout >>= putMVar outV
      else do
        liftIO_ $ forkIO $ getContent errH >>= putMVar errV
        liftIO_ $ forkIO $ foldHandleLines start cb outH >>= putMVar outV

    errs <- liftIO $ takeMVar errV
    ex <- liftIO $ waitForProcess procH

    let code = case ex of
                 ExitSuccess -> 0
                 ExitFailure n -> n

    put $ state { sStderr = errs , sCode = code }

    liftIO $ case ex of
      ExitSuccess   -> takeMVar outV
      ExitFailure n -> throwIO $ RunFailed exe args n errs

-- | The output of last external command. See "run".
lastStderr :: ShIO Text
lastStderr = gets sStderr

-- | set the stdin to be used and cleared by the next "run".
setStdin :: Text -> ShIO ()
setStdin input = modify $ \st -> st { sStdin = Just input }

-- | Pipe operator. set the stdout the first command as the stdin of the second.
(-|-) :: ShIO Text -> ShIO b -> ShIO b
one -|- two = do
  res <- (print_stdout False) one
  setStdin res
  two

-- | Copy a file, or a directory recursively.
cp_r :: FilePath -> FilePath -> ShIO ()
cp_r from to = do
    trace $ "cp -r " `mappend` toTextIgnore from `mappend` " " `mappend` toTextIgnore to
    from_d <- (test_d from)
    if not from_d then cp from to else do
         let fromName = filename from
         let toDir = if filename to == fromName then to else to FP.</> fromName
         unlessM (test_d toDir) $ mkdir toDir
         ls from >>= mapM_
            (\item -> cp_r (from FP.</> filename item) (toDir FP.</> filename item))

-- | Copy a file. The second path could be a directory, in which case the
-- original file name is used, in that directory.
cp :: FilePath -> FilePath -> ShIO ()
cp from to = do
  from' <- absPath from
  to' <- absPath to
  trace $ "cp " `mappend` toTextIgnore from' `mappend` " " `mappend` toTextIgnore to'
  to_dir <- test_d to
  let to_loc = if to_dir then to' FP.</> filename from else to'
  liftIO $ copyFile from' to_loc `catchany` (\e -> throwIO $
	  ReThrownException e (extraMsg to_loc from')
	)
  where
    extraMsg t f = "during copy from: " ++ unpack f ++ " to: " ++ unpack t

-- | for 'grep'
class PredicateLike pattern hay where
  match :: pattern -> hay -> Bool

instance PredicateLike (a -> Bool) a where
  match = id

instance (Eq a) => PredicateLike [a] [a] where
  match pat = (pat `isInfixOf`)

-- | Like filter, but more conveniently used with String lists, where a
-- substring match (TODO: also provide globs) is expressed as
--  @grep \"needle\" [ \"the\", \"stack\", \"of\", \"hay\" ]@. Boolean
-- predicates just like with "filter" are supported too:
-- @grep (\"fun\" `isPrefixOf`) [...]@.
grep :: (PredicateLike pattern hay) => pattern -> [hay] -> [hay]
grep p = filter (match p)

-- | A functor-lifting function composition.
(<$$>) :: (Functor m) => (b -> c) -> (a -> m b) -> a -> m c
f <$$> v = fmap f . v

-- | Create a temporary directory and pass it as a parameter to a ShIO
-- computation. The directory is nuked afterwards.
withTmpDir :: (FilePath -> ShIO a) -> ShIO a
withTmpDir act = do
  trace "withTmpDir"
  dir <- liftIO getTemporaryDirectory
  tid <- liftIO myThreadId
  (pS, handle) <- liftIO $ openTempFile dir ("tmp"++filter isAlphaNum (show tid))
  let p = pack pS
  liftIO $ hClose handle -- required on windows
  rm_f p
  mkdir p
  a <- act p `catchany_sh` \e -> do
	rm_rf p >> liftIO (throwIO e)
  rm_rf p
  return a

-- | Write a Lazy Text to a file.
writefile :: FilePath -> Text -> ShIO ()
writefile f bits = absPath f >>= \f' -> do
  trace $ "writefile " `mappend` toTextIgnore f'
  liftIO (TIO.writeFile (unpack f') bits)

-- | Append a Lazy Text to a file.
appendfile :: FilePath -> Text -> ShIO ()
appendfile f bits = absPath f >>= \f' -> do
  trace $ "appendfile " `mappend` toTextIgnore f'
  liftIO (TIO.appendFile (unpack f') bits)

-- | (Strictly) read file into a Text.
-- All other functions use Lazy Text.
-- So Internally this reads a file as strict text and then converts it to lazy text, which is inefficient
readfile :: FilePath -> ShIO Text
readfile = absPath >=> \fp -> do
  trace $ "readfile " `mappend` toTextIgnore fp
  (fmap LT.fromStrict . liftIO . STIO.readFile . unpack) fp


data Timing = Timing Double deriving (Read, Show, Ord, Eq)

-- | Run a ShIO computation and collect timing  information.
time :: ShIO a -> ShIO (Timing, a)
time what = sub $ do
  trace "time"
  t <- liftIO getCurrentTime
  res <- what
  t' <- liftIO getCurrentTime
  let mt = Timing (realToFrac $ diffUTCTime t' t)
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
