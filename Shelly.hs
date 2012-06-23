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
         , ls, lsT, test_e, test_f, test_d, test_s, which

         -- * Filename helpers
         , absPath, (</>), (<.>), canonic, canonicalize, relPath, relativeTo, path 

         -- * Manipulating filesystem.
         , mv, rm, rm_f, rm_rf, cp, cp_r, mkdir, mkdir_p
         , readfile, writefile, appendfile, withTmpDir

         -- * exiting the program
         , exit, errorExit, terror

         -- * Utilities.
         , (<$>), (<$$>), whenM, unlessM
         , catchany, catch_sh, finally_sh, ShellyHandler(..), catches_sh, catchany_sh
         , time
         , RunFailed(..)

         -- * convert between Text and FilePath
         , toTextIgnore, toTextWarn, fromText

         -- * Re-exported for your convenience
         , liftIO, when, unless, FilePath

         -- * internal functions for writing extension
         , get, put
         ) where

import Shelly.Base
import Shelly.Find (find)
import Control.Monad ( when, unless )
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Reader (runReaderT, ask)
import Prelude hiding ( catch, readFile, FilePath )
import Data.Char( isAlphaNum, isSpace )
import Data.Typeable
import Data.IORef
import Data.Maybe
import System.IO hiding ( readFile, FilePath )
import System.Exit
import System.Environment
import Control.Applicative
import Control.Exception hiding (handle)
import Control.Concurrent
import Data.Time.Clock( getCurrentTime, diffUTCTime  )

import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.IO as STIO
import System.Process( CmdSpec(..), StdStream(CreatePipe), CreateProcess(..), createProcess, waitForProcess, ProcessHandle )
import System.IO.Error (isPermissionError)

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text as T
import Data.Monoid (mappend)

import Filesystem.Path.CurrentOS hiding (concat, fromText, (</>), (<.>))
import Filesystem hiding (canonicalizePath)
import qualified Filesystem.Path.CurrentOS as FP

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
    cmdAll fp args = run_ fp args

instance (ShellArg arg, ShellCommand result) => ShellCommand (arg -> result) where
    cmdAll fp acc = \x -> cmdAll fp (acc ++ [toTextArg x])

-- | variadic argument version of run.
-- The syntax is more convenient, but more importantly it also allows the use of a FilePath as a command argument.
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

-- | same as 'trace', but use it combinator style
tag :: ShIO a -> Text -> ShIO a
tag action msg = do
  trace msg
  action

put :: State -> ShIO ()
put newState = do
  stateVar <- ask 
  liftIO (writeIORef stateVar newState)

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

-- | Catch an exception in the ShIO monad.
catch_sh :: (Exception e) => ShIO a -> (e -> ShIO a) -> ShIO a
catch_sh action handle = do
    ref <- ask
    liftIO $ catch (runReaderT action ref) (\e -> runReaderT (handle e) ref)

-- | Catch an exception in the ShIO monad.
finally_sh :: ShIO a -> ShIO b -> ShIO a
finally_sh action handle = do
    ref <- ask
    liftIO $ finally (runReaderT action ref) (runReaderT handle ref)


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
-- track of its own working directory and builds absolute paths internally
-- instead of passing down relative paths. This may have performance
-- repercussions if you are doing hundreds of thousands of filesystem
-- operations. You will want to handle these issues differently in those cases.
cd :: FilePath -> ShIO ()
cd = absPath >=> \dir -> do
            trace $ "cd " `mappend` toTextIgnore dir
            modify $ \st -> st { sDirectory = dir }

-- | "cd", execute a ShIO action in the new directory and then pop back to the original directory
chdir :: FilePath -> ShIO a -> ShIO a
chdir dir action = do
  d <- gets sDirectory
  cd dir
  action `finally_sh` cd d

  
-- | apply a String IO operations to a Text FilePath
{-
liftStringIO :: (String -> IO String) -> FilePath -> ShIO FilePath
liftStringIO f = liftIO . f . unpack >=> return . pack

-- | @asString f = pack . f . unpack@
asString :: (String -> String) -> FilePath -> FilePath
asString f = pack . f . unpack
-}

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
lsT :: FilePath -> ShIO [Text]
lsT = ls >=> mapM toTextWarn

-- | Obtain the current (ShIO) working directory.
pwd :: ShIO FilePath
pwd = gets sDirectory `tag` "pwd"

exit :: Int -> ShIO ()
exit 0 = liftIO (exitWith ExitSuccess) `tag` "exit 0"
exit n = liftIO (exitWith (ExitFailure n)) `tag` ("exit " `mappend` LT.pack (show n))

errorExit :: Text -> ShIO ()
errorExit msg = echo msg >> exit 1

-- | fail that takes a Text
terror :: Text -> ShIO a
terror = fail . LT.unpack

-- | Create a new directory (fails if the directory exists).
mkdir :: FilePath -> ShIO ()
mkdir = absPath >=> \fp -> do
  trace $ "mkdir " `mappend` toTextIgnore fp
  liftIO $ createDirectory False fp

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

-- | A monadic-conditional version of the "when" guard.
whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= \res -> when res a

-- | A monadic-conditional version of the "unless" guard.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= \res -> unless res a

-- | Does a path point to an existing filesystem object?
test_e :: FilePath -> ShIO Bool
test_e = absPath >=> \f -> do
  liftIO $ do
    file <- isFile f
    if file then return True else isDirectory f

-- | Does a path point to an existing file?
test_f :: FilePath -> ShIO Bool
test_f = absPath >=> liftIO . isFile

-- | A swiss army cannon for removing things. Actually this goes farther than a
-- normal rm -rf, as it will circumvent permission problems for the files we
-- own. Use carefully.
-- Uses 'removeTree'
rm_rf :: FilePath -> ShIO ()
rm_rf = absPath >=> \f -> do
  trace $ "rm -rf " `mappend` toTextIgnore f
  isDir <- (test_d f)
  if not isDir then whenM (test_f f) $ rm_f f
    else
      (liftIO_ $ removeTree f) `catch_sh` (\(e :: IOError) ->
        when (isPermissionError e) $ do
          find f >>= mapM_ (\file -> liftIO_ $ fixPermissions (unpack file) `catchany` \_ -> return ())
          liftIO $ removeTree f
        )
  where fixPermissions file =
          do permissions <- liftIO $ getPermissions file
             let deletable = permissions { readable = True, writable = True, executable = True }
             liftIO $ setPermissions file deletable

-- | Remove a file. Does not fail if the file does not exist.
-- Does fail if the file is not a file.
rm_f :: FilePath -> ShIO ()
rm_f = absPath >=> \f -> do
  trace $ "rm -f " `mappend` toTextIgnore f
  whenM (test_e f) $ canonic f >>= liftIO . removeFile

-- | Remove a file.
-- Does fail if the file does not exist (use 'rm_f' instead) or is not a file.
rm :: FilePath -> ShIO ()
rm = absPath >=> \f -> do
  trace $ "rm" `mappend` toTextIgnore f
  canonic f >>= liftIO . removeFile

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
appendToPath = absPath >=> \filepath -> do
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

-- | Create a sub-ShIO with stdout printing on or off
print_stdout :: Bool -> ShIO a -> ShIO a
print_stdout shouldPrint a = sub $ modify (\x -> x { sPrintStdout = shouldPrint }) >> a


-- | Create a sub-ShIO with command echoing on or off
print_commands :: Bool -> ShIO a -> ShIO a
print_commands shouldPrint a = sub $ modify (\st -> st { sPrintCommands = shouldPrint }) >> a

-- | Enter a sub-ShIO that inherits the environment
-- The original state will be restored when the sub-ShIO completes.
-- Exceptions are propagated normally.
sub :: ShIO a -> ShIO a
sub a = do
  oldState <- get
  modify $ \st -> st { sTrace = B.fromText "" }
  a `finally_sh` (restoreState oldState)
  where
    restoreState oldState = do
      newState <- get
      put oldState { sTrace = sTrace oldState `mappend` sTrace newState  }

-- | Create a sub-ShIO with shell character escaping on or off
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
-- This interface is crude, but it works for now.
--
-- Please note this sets 'escaping' to False: the commands will not be shell escaped.
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
cp_r from' to' = do
    fromDir <- absPath from'
    from_d <- (test_d fromDir)
    if not from_d then cp from' to' else do
       to <- absPath to'
       trace $ "cp -r " `mappend` toTextIgnore fromDir `mappend` " " `mappend` toTextIgnore to
       toIsDir <- test_d to
       toDir <- if toIsDir
               then return $ fromDir </> to
               else mkdir to >> return to
       when (fromDir == toDir) $ liftIO $ throwIO $ userError $ LT.unpack $ "cp_r: " `mappend`
         toTextIgnore fromDir `mappend` " and " `mappend` toTextIgnore toDir `mappend` " are identical"

       ls fromDir >>= mapM_ (\item -> cp_r (fromDir FP.</> filename item) (toDir FP.</> filename item))

-- | Copy a file. The second path could be a directory, in which case the
-- original file name is used, in that directory.
cp :: FilePath -> FilePath -> ShIO ()
cp from' to' = do
  from <- absPath from'
  to <- absPath to'
  trace $ "cp " `mappend` toTextIgnore from `mappend` " " `mappend` toTextIgnore to
  to_dir <- test_d to
  let to_loc = if to_dir then to FP.</> filename from else to
  liftIO $ copyFile from to_loc `catchany` (\e -> throwIO $
	  ReThrownException e (extraMsg to_loc from)
	)
  where
    extraMsg t f = "during copy from: " ++ unpack f ++ " to: " ++ unpack t

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
  act p `finally_sh` (rm_rf p)

-- | Write a Lazy Text to a file.
writefile :: FilePath -> Text -> ShIO ()
writefile f' bits = absPath f' >>= \f -> do
  trace $ "writefile " `mappend` toTextIgnore f
  liftIO (TIO.writeFile (unpack f) bits)

-- | Append a Lazy Text to a file.
appendfile :: FilePath -> Text -> ShIO ()
appendfile f' bits = absPath f' >>= \f -> do
  trace $ "appendfile " `mappend` toTextIgnore f
  liftIO (TIO.appendFile (unpack f) bits)

-- | (Strictly) read file into a Text.
-- All other functions use Lazy Text.
-- So Internally this reads a file as strict text and then converts it to lazy text, which is inefficient
readfile :: FilePath -> ShIO Text
readfile = absPath >=> \fp -> do
  trace $ "readfile " `mappend` toTextIgnore fp
  (fmap LT.fromStrict . liftIO . STIO.readFile . unpack) fp


-- | Run a ShIO computation and collect timing  information.
time :: ShIO a -> ShIO (Double, a)
time what = sub $ do
  trace "time"
  t <- liftIO getCurrentTime
  res <- what
  t' <- liftIO getCurrentTime
  return ((realToFrac $ diffUTCTime t' t), res)

{- memory stats never implemented
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
