{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings,
             MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances,
             TypeFamilies, IncoherentInstances, GADTs #-}

-- | A module for shell-like programming in Haskell.
-- Shelly's focus is entirely on ease of use for those coming from shell scripting.
-- However, it also tries to use modern libraries and techniques to keep things efficient.
--
-- The functionality provided by
-- this module is (unlike standard Haskell filesystem functionality)
-- thread-safe: each Sh maintains its own environment and its own working
-- directory.
--
-- Recommended usage includes putting the following at the top of your program,
-- otherwise you will likely need either type annotations or type conversions
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- > import Shelly
-- > import qualified Data.Text as T
-- > default (T.Text)
module Shelly
       (
         -- * Entering Sh.
         Sh, ShIO, shelly, shellyNoDir, sub, silently, verbosely, escaping, print_stdout, print_commands, tracing, errExit

         -- * Running external commands.
         , run, run_, runFoldLines, cmd, FoldCallback
         , (-|-), lastStderr, setStdin, lastExitCode
         , command, command_, command1, command1_
         , sshPairs, sshPairs_
         , ShellArg (..)

         -- * Running commands Using handles
         , runHandle, runHandles, transferLinesAndCombine, transferFoldHandleLines
         , StdHandle(..), StdStream(..)


         -- * Modifying and querying environment.
         , setenv, get_env, get_env_text, getenv, get_env_def, appendToPath

         -- * Environment directory
         , cd, chdir, pwd

         -- * Printing
         , echo, echo_n, echo_err, echo_n_err, inspect, inspect_err
         , tag, trace, show_command

         -- * Querying filesystem.
         , ls, lsT, test_e, test_f, test_d, test_s, which

         -- * Filename helpers
         , absPath, (</>), (<.>), canonic, canonicalize, relPath, relativeTo, path
         , hasExt

         -- * Manipulating filesystem.
         , mv, rm, rm_f, rm_rf, cp, cp_r, mkdir, mkdir_p, mkdirTree

         -- * reading/writing Files
         , readfile, readBinary, writefile, appendfile, touchfile, withTmpDir

         -- * exiting the program
         , exit, errorExit, quietExit, terror

         -- * Exceptions
         , catchany, catch_sh, finally_sh, ShellyHandler(..), catches_sh, catchany_sh

         -- * convert between Text and FilePath
         , toTextIgnore, toTextWarn, fromText

         -- * Utility Functions
         , whenM, unlessM, time, sleep 

         -- * Re-exported for your convenience
         , liftIO, when, unless, FilePath, (<$>)

         -- * internal functions for writing extensions
         , get, put

         -- * find functions
         , find, findWhen, findFold, findDirFilter, findDirFilterWhen, findFoldDirFilter
         ) where

import Shelly.Base
import Shelly.Find
import Control.Monad ( when, unless, void, forM, filterM )
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Reader (ask)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding ( readFile, FilePath, catch)
#else
import Prelude hiding ( readFile, FilePath)
#endif
import Data.Char ( isAlphaNum, isSpace )
import Data.Typeable
import Data.IORef
import Data.Sequence (Seq, (|>))
import Data.Foldable (toList)
import Data.Maybe
import System.IO ( hClose, stderr, stdout, openTempFile )
import System.IO.Error (isPermissionError, catchIOError, isEOFError, isIllegalOperation)
import System.Exit
import System.Environment
import Control.Applicative
import Control.Exception hiding (handle)
import Control.Concurrent
import Data.Time.Clock( getCurrentTime, diffUTCTime  )

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Process( CmdSpec(..), StdStream(CreatePipe, UseHandle), CreateProcess(..), createProcess, waitForProcess, terminateProcess, ProcessHandle, StdStream(..) )

import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Data.Monoid (Monoid, mempty, mappend)
#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#else
import Data.Monoid ((<>))
#endif

import Filesystem.Path.CurrentOS hiding (concat, fromText, (</>), (<.>))
import Filesystem hiding (canonicalizePath)
import qualified Filesystem.Path.CurrentOS as FP

import System.Directory ( setPermissions, getPermissions, Permissions(..), getTemporaryDirectory )
import Data.Char (isDigit)

import Data.Tree(Tree(..))
import qualified Data.Set as S
import qualified Data.List as L

searchPathSeparator :: Char
#if defined(mingw32_HOST_OS)
searchPathSeparator = ';'
#else
searchPathSeparator = ':'
#endif

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

cmd :: (ShellArgs args) => FilePath -> args -> Sh Text
cmd fp args = run fp $ toTextArgs args
-}

-- | Argument converter for the variadic argument version of 'run' called 'cmd'.
-- Useful for a type signature of a function that uses 'cmd'
class ShellArg a where toTextArg :: a -> Text
instance ShellArg Text     where toTextArg = id
instance ShellArg FilePath where toTextArg = toTextIgnore
instance ShellArg String   where toTextArg = T.pack

-- | used to create the variadic function 'cmd'
class ShellCommand t where
    cmdAll :: FilePath -> [Text] -> t

instance ShellCommand (Sh Text) where
    cmdAll = run

instance (s ~ Text, Show s) => ShellCommand (Sh s) where
    cmdAll = run

-- note that Sh () actually doesn't work for its case (_<- cmd) when there is no type signature
instance ShellCommand (Sh ()) where
    cmdAll = run_

instance (ShellArg arg, ShellCommand result) => ShellCommand (arg -> result) where
    cmdAll fp acc x = cmdAll fp (acc ++ [toTextArg x])


-- | variadic argument version of 'run'.
-- Please see the documenation for 'run'.
--
-- The syntax is more convenient, but more importantly it also allows the use of a FilePath as a command argument.
-- So an argument can be a Text or a FilePath without manual conversions.
-- a FilePath is automatically converted to Text with 'toTextIgnore'.
--
-- Convenient usage of 'cmd' requires the following:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- > import Shelly
-- > import qualified Data.Text as T
-- > default (T.Text)
--
cmd :: (ShellCommand result) => FilePath -> result
cmd fp = cmdAll fp []

-- | Helper to convert a Text to a FilePath. Used by '(</>)' and '(<.>)'
class ToFilePath a where
  toFilePath :: a -> FilePath

instance ToFilePath FilePath where toFilePath = id
instance ToFilePath Text     where toFilePath = FP.fromText
instance ToFilePath String   where toFilePath = FP.fromText . T.pack


-- | uses System.FilePath.CurrentOS, but can automatically convert a Text
(</>) :: (ToFilePath filepath1, ToFilePath filepath2) => filepath1 -> filepath2 -> FilePath
x </> y = toFilePath x FP.</> toFilePath y

-- | uses System.FilePath.CurrentOS, but can automatically convert a Text
(<.>) :: (ToFilePath filepath) => filepath -> Text -> FilePath
x <.> y = toFilePath x FP.<.> y



toTextWarn :: FilePath -> Sh Text
toTextWarn efile = case toText efile of
    Left f -> encodeError f >> return f
    Right f -> return f
  where
    encodeError f = echo ("Invalid encoding for file: " <> f)

fromText :: Text -> FilePath
fromText = FP.fromText

-- | Transfer from one handle to another
-- For example, send contents of a process output to stdout.
-- does not close the write handle.
--
-- Also, return the complete contents being streamed line by line.
transferLinesAndCombine :: Handle -> Handle -> IO Text
transferLinesAndCombine h1 h2 = transferFoldHandleLines mempty (|>) h1 h2 >>=
    return . lineSeqToText

lineSeqToText :: Seq Text -> Text
-- extra append puts a newline at the end
lineSeqToText = T.intercalate "\n" . toList . flip (|>) ""

type FoldCallback a = (a -> Text -> a)

-- | Transfer from one handle to another
-- For example, send contents of a process output to stdout.
-- does not close the write handle.
--
-- Also, fold over the contents being streamed line by line
transferFoldHandleLines :: a -> FoldCallback a -> Handle -> Handle -> IO a
transferFoldHandleLines start foldLine readHandle writeHandle = go start
  where
    catchIOErrors action = catchIOError
                   (fmap Just action)
                   (\e -> if isEOFError e || isIllegalOperation e -- handle was closed
                           then return Nothing
                           else ioError e)
    go acc = do
        mLine <- catchIOErrors (TIO.hGetLine readHandle)
        case mLine of
            Nothing -> return acc
            Just line -> TIO.hPutStrLn writeHandle line >> go (foldLine acc line)

foldHandleLines :: a -> FoldCallback a -> Handle -> IO a
foldHandleLines start foldLine readHandle = go start
  where
    go acc = do
      line <- TIO.hGetLine readHandle
      go $ foldLine acc line
     `catchany` \_ -> return acc

-- | same as 'trace', but use it combinator style
tag :: Sh a -> Text -> Sh a
tag action msg = do
  trace msg
  action

put :: State -> Sh ()
put newState = do
  stateVar <- ask
  liftIO (writeIORef stateVar newState)

runCommandNoEscape :: [StdHandle] -> State -> FilePath -> [Text] -> Sh (Handle, Handle, Handle, ProcessHandle)
runCommandNoEscape handles st exe args = liftIO $ shellyProcess handles st $
    ShellCommand $ T.unpack $ T.intercalate " " (toTextIgnore exe : args)

runCommand :: [StdHandle] -> State -> FilePath -> [Text] -> Sh (Handle, Handle, Handle, ProcessHandle)
runCommand handles st exe args = findExe exe >>= \fullExe ->
  liftIO $ shellyProcess handles st $
    RawCommand (encodeString fullExe) (map T.unpack args)
  where
    findExe :: FilePath -> Sh FilePath
    findExe fp = do
        mExe <- which exe
        case mExe of
          Just execFp -> return execFp
#if defined(mingw32_HOST_OS)
          -- windows looks in extra places besides the PATH, so just give
          -- up even if the behavior is not properly specified anymore
          Nothing -> return fp
#else
          Nothing -> liftIO $ throwIO $ userError $
            "shelly did not find " `mappend` encodeString fp `mappend`
              if absolute exe then "" else " in the PATH"
#endif




shellyProcess :: [StdHandle] -> State -> CmdSpec -> IO (Handle, Handle, Handle, ProcessHandle)
shellyProcess reusedHandles st cmdSpec =  do
    (createdInH, createdOutH, createdErrorH, pHandle) <- createProcess CreateProcess {
          cmdspec = cmdSpec
        , cwd = Just $ encodeString $ sDirectory st
        , env = Just $ sEnvironment st
        , std_in  = createUnless mInH
        , std_out = createUnless mOutH
        , std_err = createUnless mErrorH
        , close_fds = False
#if MIN_VERSION_process(1,1,0)
        , create_group = False
#endif
        }
    return ( just $ createdInH <|> toHandle mInH
           , just $ createdOutH <|> toHandle mOutH
           , just $ createdErrorH <|> toHandle mErrorH
           , pHandle
           )
  where
    just :: Maybe a -> a
    just Nothing  = error "error in shelly creating process"
    just (Just j) = j

    toHandle (Just (UseHandle h)) = Just h
    toHandle (Just CreatePipe)    = error "shelly process creation failure CreatePIpe"
    toHandle (Just Inherit)       = error "cannot access an inherited pipe"
    toHandle Nothing              = error "error in shelly creating process"

    createUnless Nothing = CreatePipe
    createUnless (Just stream) = stream

    mInH    = getStream mIn reusedHandles
    mOutH   = getStream mOut reusedHandles
    mErrorH = getStream mError reusedHandles

    getStream :: (StdHandle -> Maybe StdStream) -> [StdHandle] -> Maybe StdStream
    getStream _ [] = Nothing
    getStream mHandle (h:hs) = mHandle h <|> getStream mHandle hs

    mIn, mOut, mError :: (StdHandle -> Maybe StdStream)
    mIn (InHandle h) = Just h
    mIn _ = Nothing
    mOut (OutHandle h) = Just h
    mOut _ = Nothing
    mError (ErrorHandle h) = Just h
    mError _ = Nothing

{-
-- | use for commands requiring usage of sudo. see 'run_sudo'.
--  Use this pattern for priveledge separation
newtype Sudo a = Sudo { sudo :: Sh a }

-- | require that the caller explicitly state 'sudo'
run_sudo :: Text -> [Text] -> Sudo Text
run_sudo cmd args = Sudo $ run "/usr/bin/sudo" (cmd:args)
-}

-- | Same as a normal 'catch' but specialized for the Sh monad.
catch_sh :: (Exception e) => Sh a -> (e -> Sh a) -> Sh a
catch_sh action handle = do
    ref <- ask
    liftIO $ catch (runSh action ref) (\e -> runSh (handle e) ref)

-- | Same as a normal 'finally' but specialized for the 'Sh' monad.
finally_sh :: Sh a -> Sh b -> Sh a
finally_sh action handle = do
    ref <- ask
    liftIO $ finally (runSh action ref) (runSh handle ref)


-- | You need to wrap exception handlers with this when using 'catches_sh'.
data ShellyHandler a = forall e . Exception e => ShellyHandler (e -> Sh a)

-- | Same as a normal 'catches', but specialized for the 'Sh' monad.
catches_sh :: Sh a -> [ShellyHandler a] -> Sh a
catches_sh action handlers = do
    ref <- ask
    let runner a = runSh a ref
    liftIO $ catches (runner action) $ map (toHandler runner) handlers
  where
    toHandler :: (Sh a -> IO a) -> ShellyHandler a -> Handler a
    toHandler runner (ShellyHandler handle) = Handler (\e -> runner (handle e))

-- | Catch an exception in the Sh monad.
catchany_sh :: Sh a -> (SomeException -> Sh a) -> Sh a
catchany_sh = catch_sh

-- | Change current working directory of Sh. This does *not* change the
-- working directory of the process we are running it. Instead, Sh keeps
-- track of its own working directory and builds absolute paths internally
-- instead of passing down relative paths.
cd :: FilePath -> Sh ()
cd = canonic >=> cd'
  where
    cd' dir = do
        trace $ "cd " <> tdir
        unlessM (test_d dir) $ errorExit $ "not a directory: " <> tdir
        modify $ \st -> st { sDirectory = dir, sPathExecutables = Nothing }
      where
        tdir = toTextIgnore dir

-- | 'cd', execute a Sh action in the new directory and then pop back to the original directory
chdir :: FilePath -> Sh a -> Sh a
chdir dir action = do
  d <- gets sDirectory
  cd dir
  action `finally_sh` cd d

-- | 'chdir', but first create the directory if it does not exit
chdir_p :: FilePath -> Sh a -> Sh a
chdir_p d action = mkdir_p d >> chdir d action


-- | apply a String IO operations to a Text FilePath
{-
liftStringIO :: (String -> IO String) -> FilePath -> Sh FilePath
liftStringIO f = liftIO . f . unpack >=> return . pack

-- | @asString f = pack . f . unpack@
asString :: (String -> String) -> FilePath -> FilePath
asString f = pack . f . unpack
-}

pack :: String -> FilePath
pack = decodeString

-- | Move a file. The second path could be a directory, in which case the
-- original file is moved into that directory.
-- wraps system-fileio 'FileSystem.rename', which may not work across FS boundaries
mv :: FilePath -> FilePath -> Sh ()
mv from' to' = do
  from <- absPath from'
  to <- absPath to'
  trace $ "mv " <> toTextIgnore from <> " " <> toTextIgnore to
  to_dir <- test_d to
  let to_loc = if not to_dir then to else to FP.</> filename from
  liftIO $ rename from to_loc
    `catchany` (\e -> throwIO $
      ReThrownException e (extraMsg to_loc from)
    )
  where
    extraMsg t f = "during copy from: " ++ encodeString f ++ " to: " ++ encodeString t

-- | Get back [Text] instead of [FilePath]
lsT :: FilePath -> Sh [Text]
lsT = ls >=> mapM toTextWarn

-- | Obtain the current (Sh) working directory.
pwd :: Sh FilePath
pwd = gets sDirectory `tag` "pwd"

-- | exit 0 means no errors, all other codes are error conditions
exit :: Int -> Sh a
exit 0 = liftIO exitSuccess `tag` "exit 0"
exit n = liftIO (exitWith (ExitFailure n)) `tag` ("exit " <> T.pack (show n))

-- | echo a message and exit with status 1
errorExit :: Text -> Sh a
errorExit msg = echo msg >> exit 1

-- | for exiting with status > 0 without printing debug information
quietExit :: Int -> Sh a
quietExit 0 = exit 0
quietExit n = throw $ QuietExit n

-- | fail that takes a Text
terror :: Text -> Sh a
terror = fail . T.unpack

-- | Create a new directory (fails if the directory exists).
mkdir :: FilePath -> Sh ()
mkdir = absPath >=> \fp -> do
  trace $ "mkdir " <> toTextIgnore fp
  liftIO $ createDirectory False fp

-- | Create a new directory, including parents (succeeds if the directory
-- already exists).
mkdir_p :: FilePath -> Sh ()
mkdir_p = absPath >=> \fp -> do
  trace $ "mkdir -p " <> toTextIgnore fp
  liftIO $ createTree fp

-- | Create a new directory tree. You can describe a bunch of directories as 
-- a tree and this function will create all subdirectories. An example:
--
-- > exec = mkTree $
-- >           "package" # [
-- >                "src" # [
-- >                    "Data" # leaves ["Tree", "List", "Set", "Map"] 
-- >                ],
-- >                "test" # leaves ["QuickCheck", "HUnit"],
-- >                "dist/doc/html" # []
-- >            ]
-- >         where (#) = Node
-- >               leaves = map (# []) 
--
mkdirTree :: Tree FilePath -> Sh ()
mkdirTree = mk . unrollPath 
    where mk :: Tree FilePath -> Sh ()
          mk (Node a ts) = do
            b <- test_d a
            unless b $ mkdir a
            chdir a $ mapM_ mkdirTree ts
            
          unrollPath :: Tree FilePath -> Tree FilePath
          unrollPath (Node v ts) = unrollRoot v $ map unrollPath ts
              where unrollRoot x = foldr1 phi $ map Node $ splitDirectories x
                    phi a b = a . return . b

-- | Get a full path to an executable by looking at the @PATH@ environement
-- variable. Windows normally looks in additional places besides the
-- @PATH@: this does not duplicate that behavior.
which :: FilePath -> Sh (Maybe FilePath)
which originalFp = whichFull
#if defined(mingw32_HOST_OS)
    $ case extension originalFp of
        Nothing -> originalFp <.> "exe"
        Just _ -> originalFp
#else
    originalFp
#endif
  where
    whichFull fp = do
      (trace . mappend "which " . toTextIgnore) fp >> whichUntraced
      where
        whichUntraced | absolute fp                      = checkFile
                      | length (splitDirectories fp) > 0 = lookupPath
                      | otherwise                        = lookupCache
        checkFile = do
            exists <- liftIO $ isFile fp
            return $ toMaybe exists fp

        lookupPath =
            (pathDirs >>=) $ findMapM $ \dir -> do
                let fullFp = dir </> fp
                res <- liftIO $ isExecutable $ encodeString $ fullFp
                return $ toMaybe res fullFp

        lookupCache = do
            pathExecutables <- cachedPathExecutables
            liftIO $ print pathExecutables
            return $ fmap (flip (</>) fp . fst) $
                L.find (S.member fp . snd) pathExecutables
        

        pathDirs = mapM absPath =<< ((map fromText . T.split (== searchPathSeparator)) `fmap` get_env_text "PATH")

        isExecutable f = (executable `fmap` getPermissions f) `catch`
                           (\(_ :: IOError) -> return False)

        cachedPathExecutables :: Sh [(FilePath, S.Set FilePath)]
        cachedPathExecutables = do
          mPathExecutables <- gets sPathExecutables
          case mPathExecutables of
              Just pExecutables -> return pExecutables
              Nothing -> do
                dirs <- pathDirs
                executables <- forM dirs (\dir -> do
                    files <- (liftIO . listDirectory) dir `catch_sh` (\(_ :: IOError) -> return [])
                    exes <- fmap (map snd) $ liftIO $ filterM (isExecutable . fst) $
                      map (\f -> (encodeString f, filename f)) files
                    return $ S.fromList exes
                  )
                let cachedExecutables = zip dirs executables
                modify $ \x -> x { sPathExecutables = Just cachedExecutables }
                return $ cachedExecutables


-- | Returns 'Just' if the precondition is fulfilled.
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True x = Just x

-- | A monadic findMap, taken from MissingM package
findMapM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findMapM _ [] = return Nothing
findMapM f (x:xs) = do
    mb <- f x
    if (isJust mb)
      then return mb
      else findMapM f xs

-- | A monadic-conditional version of the 'unless' guard.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= \res -> unless res a

-- | Does a path point to an existing filesystem object?
test_e :: FilePath -> Sh Bool
test_e = absPath >=> \f ->
  liftIO $ do
    file <- isFile f
    if file then return True else isDirectory f

-- | Does a path point to an existing file?
test_f :: FilePath -> Sh Bool
test_f = absPath >=> liftIO . isFile

-- | A swiss army cannon for removing things. Actually this goes farther than a
-- normal rm -rf, as it will circumvent permission problems for the files we
-- own. Use carefully.
-- Uses 'removeTree'
rm_rf :: FilePath -> Sh ()
rm_rf = absPath >=> \f -> do
  trace $ "rm -rf " <> toTextIgnore f
  isDir <- (test_d f)
  if not isDir then whenM (test_f f) $ rm_f f
    else
      (liftIO_ $ removeTree f) `catch_sh` (\(e :: IOError) ->
        when (isPermissionError e) $ do
          find f >>= mapM_ (\file -> liftIO_ $ fixPermissions (encodeString file) `catchany` \_ -> return ())
          liftIO $ removeTree f
        )
  where fixPermissions file =
          do permissions <- liftIO $ getPermissions file
             let deletable = permissions { readable = True, writable = True, executable = True }
             liftIO $ setPermissions file deletable

-- | Remove a file. Does not fail if the file does not exist.
-- Does fail if the file is not a file.
rm_f :: FilePath -> Sh ()
rm_f = absPath >=> \f -> do
  trace $ "rm -f " <> toTextIgnore f
  whenM (test_e f) $ canonic f >>= liftIO . removeFile

-- | Remove a file.
-- Does fail if the file does not exist (use 'rm_f' instead) or is not a file.
rm :: FilePath -> Sh ()
rm = absPath >=> \f -> do
  trace $ "rm" <> toTextIgnore f
  -- TODO: better error message for removeFile (give filename)
  canonic f >>= liftIO . removeFile

-- | Set an environment variable. The environment is maintained in Sh
-- internally, and is passed to any external commands to be executed.
setenv :: Text -> Text -> Sh ()
setenv k v = if k == path_env then setPath v else setenvRaw k v

setenvRaw :: Text -> Text -> Sh ()
setenvRaw k v = modify $ \x -> x { sEnvironment = wibble $ sEnvironment x }
  where
    (kStr, vStr) = (T.unpack k, T.unpack v)
    wibble environment = (kStr, vStr) : filter ((/=kStr) . fst) environment

setPath :: Text -> Sh ()
setPath newPath = do
  modify $ \x -> x{ sPathExecutables = Nothing }
  setenvRaw path_env newPath

path_env :: Text
path_env = "PATH"

-- | add the filepath onto the PATH env variable
appendToPath :: FilePath -> Sh ()
appendToPath = absPath >=> \filepath -> do
  tp <- toTextWarn filepath
  pe <- get_env_text path_env
  setPath $ pe <> T.singleton searchPathSeparator <> tp

-- | Fetch the current value of an environment variable.
-- if non-existant or empty text, will be Nothing
get_env :: Text -> Sh (Maybe Text)
get_env k = do
  mval <- return . fmap T.pack . lookup (T.unpack k) =<< gets sEnvironment
  return $ case mval of
    Nothing  -> Nothing
    Just val -> toMaybe (not $ T.null val) val

-- | deprecated
getenv :: Text -> Sh Text
getenv k = get_env_def k ""
{-# DEPRECATED getenv "use get_env or get_env_text" #-}

-- | Fetch the current value of an environment variable. Both empty and
-- non-existent variables give empty string as a result.
get_env_text :: Text -> Sh Text
get_env_text = get_env_def ""

-- | Fetch the current value of an environment variable. Both empty and
-- non-existent variables give the default Text value as a result
get_env_def :: Text -> Text -> Sh Text
get_env_def d = get_env >=> return . fromMaybe d
{-# DEPRECATED get_env_def "use fromMaybe DEFAULT get_env" #-}


-- | Create a sub-Sh in which external command outputs are not echoed and
-- commands are not printed.
-- See 'sub'.
silently :: Sh a -> Sh a
silently a = sub $ modify (\x -> x { sPrintStdout = False, sPrintCommands = False }) >> a

-- | Create a sub-Sh in which external command outputs are echoed and
-- Executed commands are printed
-- See 'sub'.
verbosely :: Sh a -> Sh a
verbosely a = sub $ modify (\x -> x { sPrintStdout = True, sPrintCommands = True }) >> a

-- | Create a sub-Sh with stdout printing on or off
-- Defaults to True.
print_stdout :: Bool -> Sh a -> Sh a
print_stdout shouldPrint a = sub $ modify (\x -> x { sPrintStdout = shouldPrint }) >> a


-- | Create a sub-Sh with command echoing on or off
-- Defaults to False, set to True by 'verbosely'
print_commands :: Bool -> Sh a -> Sh a
print_commands shouldPrint a = sub $ modify (\st -> st { sPrintCommands = shouldPrint }) >> a

-- | Enter a sub-Sh that inherits the environment
-- The original state will be restored when the sub-Sh completes.
-- Exceptions are propagated normally.
sub :: Sh a -> Sh a
sub a = do
  oldState <- get
  modify $ \st -> st { sTrace = T.empty }
  a `finally_sh` restoreState oldState
  where
    restoreState oldState = do
      newState <- get
      put oldState {
         -- avoid losing the log
         sTrace  = sTrace oldState <> sTrace newState 
         -- latest command execution: not make sense to restore these to old settings
       , sCode   = sCode newState
       , sStderr = sStderr newState
         -- it is questionable what the behavior of stdin should be
       , sStdin  = sStdin newState
       }

-- | Create a sub-Sh where commands are not traced
-- Defaults to True.
-- You should only set to False temporarily for very specific reasons
tracing :: Bool -> Sh a -> Sh a
tracing shouldTrace action = sub $ do
  modify $ \st -> st { sTracing = shouldTrace }
  action

-- | Create a sub-Sh with shell character escaping on or off.
-- Defaults to @True@.
--
-- Setting to @False@ allows for shell wildcard such as * to be expanded by the shell along with any other special shell characters.
-- As a side-effect, setting to @False@ causes changes to @PATH@ to be ignored:
-- see the 'run' documentation.
escaping :: Bool -> Sh a -> Sh a
escaping shouldEscape action = sub $ do
  modify $ \st -> st { sRun =
      if shouldEscape then runCommand else runCommandNoEscape
    }
  action

-- | named after bash -e errexit. Defaults to @True@.
-- When @True@, throw an exception on a non-zero exit code.
-- When @False@, ignore a non-zero exit code.
-- Not recommended to set to @False@ unless you are specifically checking the error code with 'lastExitCode'.
errExit :: Bool -> Sh a -> Sh a
errExit shouldExit action = sub $ do
  modify $ \st -> st { sErrExit = shouldExit }
  action

data ShellyOpts = ShellyOpts { failToDir :: Bool }

-- avoid data-default dependency for now
-- instance Default ShellyOpts where 
shellyOpts :: ShellyOpts
shellyOpts = ShellyOpts { failToDir = True }

-- | Using this entry point does not create a @.shelly@ directory in the case
-- of failure. Instead it logs directly into the standard error stream (@stderr@).
shellyNoDir :: MonadIO m => Sh a -> m a
shellyNoDir = shelly' shellyOpts { failToDir = False }

-- | Enter a Sh from (Monad)IO. The environment and working directories are
-- inherited from the current process-wide values. Any subsequent changes in
-- processwide working directory or environment are not reflected in the
-- running Sh.
shelly :: MonadIO m => Sh a -> m a
shelly = shelly' shellyOpts

shelly' :: MonadIO m => ShellyOpts -> Sh a -> m a
shelly' opts action = do
  environment <- liftIO getEnvironment
  dir <- liftIO getWorkingDirectory
  let def  = State { sCode = 0
                   , sStdin = Nothing
                   , sStderr = T.empty
                   , sPrintStdout = True
                   , sPrintCommands = False
                   , sRun = runCommand
                   , sEnvironment = environment
                   , sTracing = True
                   , sTrace = T.empty
                   , sDirectory = dir
                   , sPathExecutables = Nothing
                   , sErrExit = True
                   }
  stref <- liftIO $ newIORef def
  let caught =
        action `catches_sh` [
              ShellyHandler (\ex ->
                case ex of
                  ExitSuccess   -> liftIO $ throwIO ex
                  ExitFailure _ -> throwExplainedException ex
              )
            , ShellyHandler (\ex -> case ex of
                                     QuietExit n -> liftIO $ throwIO $ ExitFailure n)
            , ShellyHandler (\(ex::SomeException) -> throwExplainedException ex)
          ]
  liftIO $ runSh caught stref
  where
    throwExplainedException :: Exception exception => exception -> Sh a
    throwExplainedException ex = get >>= errorMsg >>= liftIO . throwIO . ReThrownException ex
    errorMsg st =
      if not (failToDir opts) then ranCommands else do
          d <- pwd
          sf <- shellyFile
          let logFile = d</>shelly_dir</>sf
          (writefile logFile trc >> return ("log of commands saved to: " <> encodeString logFile))
            `catchany_sh` (\_ -> ranCommands)

      where
        trc = sTrace st
        ranCommands = return . mappend "Ran commands: \n" . T.unpack $ trc

    shelly_dir = ".shelly"
    shellyFile = chdir_p shelly_dir $ do
      fs <- ls "."
      return $ pack $ show (nextNum fs) <> ".txt"

    nextNum :: [FilePath] -> Int
    nextNum [] = 1
    nextNum fs = (+ 1) . maximum . map (readDef 1 . filter isDigit . encodeString . filename) $ fs

-- from safe package
readDef :: Read a => a -> String -> a
readDef def = fromMaybe def . readMay
  where
    readMay :: Read a => String -> Maybe a
    readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                  [x] -> Just x
                  _ -> Nothing

data RunFailed = RunFailed FilePath [Text] Int Text deriving (Typeable)

instance Show RunFailed where
  show (RunFailed exe args code errs) =
    let codeMsg = case code of
          127 -> ". exit code 127 usually means the command does not exist (in the PATH)"
          _ -> ""
    in "error running: " ++ T.unpack (show_command exe args) ++
         "\nexit status: " ++ show code ++ codeMsg ++ "\nstderr: " ++ T.unpack errs

instance Exception RunFailed

show_command :: FilePath -> [Text] -> Text
show_command exe args =
    T.intercalate " " $ map quote (toTextIgnore exe : args)
  where
    quote t | T.any (== '\'') t = t
    quote t | T.any isSpace t = surround '\'' t
    quote t | otherwise = t

surround :: Char -> Text -> Text
surround c t = T.cons c $ T.snoc t c

-- | same as 'sshPairs', but returns ()
sshPairs_ :: Text -> [(FilePath, [Text])] -> Sh ()
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
-- Internally the list of commands are combined with the string @&&@ before given to ssh.
sshPairs :: Text -> [(FilePath, [Text])] -> Sh Text
sshPairs _ [] = return ""
sshPairs server cmds = sshPairs' run server cmds

sshPairs' :: (FilePath -> [Text] -> Sh a) -> Text -> [(FilePath, [Text])] -> Sh a
sshPairs' run' server actions = escaping False $ do
    let ssh_commands = surround '\'' $ foldl1
          (\memo next -> memo <> " && " <> next)
          (map toSSH actions)
    run' "ssh" [server, ssh_commands]
  where
    toSSH (exe,args) = show_command exe args


data QuietExit = QuietExit Int deriving (Show, Typeable)
instance Exception QuietExit

data ReThrownException e = ReThrownException e String deriving (Typeable)
instance Exception e => Exception (ReThrownException e)
instance Exception e => Show (ReThrownException e) where
  show (ReThrownException ex msg) = "\n" ++
    msg ++ "\n" ++ "Exception: " ++ show ex

-- | Execute an external command.
-- Takes the command name and arguments.
--
-- You may prefer using 'cmd' instead, which is a variadic argument version
-- of this function.
--
-- 'stdout' and 'stderr' are collected. The 'stdout' is returned as
-- a result of 'run', and complete stderr output is available after the fact using
-- 'lastStderr'
--
-- All of the stdout output will be loaded into memory.
-- You can avoid this if you don't need stdout by using 'run_',
-- If you want to avoid the memory and need to process the output then use 'runFoldLines' or 'runHandle' or 'runHandles'.
--
-- By default shell characters are escaped and
-- the command name is a name of a program that can be found via @PATH@.
-- Shelly will look through the @PATH@ itself to find the command.
--
-- When 'escaping' is set to @False@, shell characters are allowed.
-- Since there is no longer a guarantee that a single program name is
-- given, Shelly cannot look in the @PATH@ for it.
-- a @PATH@ modified by setenv is not taken into account when finding the exe name.
-- Instead the original Haskell program @PATH@ is used.
-- On a Posix system the @env@ command can be used to make the 'setenv' PATH used when 'escaping' is set to False. @env echo hello@ instead of @echo hello@
--
run :: FilePath -> [Text] -> Sh Text
run fp args = return . lineSeqToText =<< runFoldLines mempty (|>) fp args

-- | bind some arguments to run for re-use. Example:
--
-- > monit = command "monit" ["-c", "monitrc"]
-- > monit ["stop", "program"]
command :: FilePath -> [Text] -> [Text] -> Sh Text
command com args more_args = run com (args ++ more_args)

-- | bind some arguments to 'run_' for re-use. Example:
--
-- > monit_ = command_ "monit" ["-c", "monitrc"]
-- > monit_ ["stop", "program"]
command_ :: FilePath -> [Text] -> [Text] -> Sh ()
command_ com args more_args = run_ com (args ++ more_args)

-- | bind some arguments to run for re-use, and require 1 argument. Example:
--
-- > git = command1 "git" []; git "pull" ["origin", "master"]
command1 :: FilePath -> [Text] -> Text -> [Text] -> Sh Text
command1 com args one_arg more_args = run com ([one_arg] ++ args ++ more_args)

-- | bind some arguments to run for re-use, and require 1 argument. Example:
--
-- > git_ = command1_ "git" []; git "pull" ["origin", "master"]
command1_ :: FilePath -> [Text] -> Text -> [Text] -> Sh ()
command1_ com args one_arg more_args = run_ com ([one_arg] ++ args ++ more_args)

-- | the same as 'run', but return @()@ instead of the stdout content
-- stdout will be read and discarded line-by-line
run_ :: FilePath -> [Text] -> Sh ()
run_ = runFoldLines () (\_ _ -> ())

liftIO_ :: IO a -> Sh ()
liftIO_ action = void (liftIO action)

-- | Similar to 'run' but gives the raw stdout handle in a callback.
-- If you want even more control, use 'runHandles'.
runHandle :: FilePath -- ^ command
          -> [Text] -- ^ arguments
          -> (Handle -> Sh a) -- ^ stdout handle
          -> Sh a
runHandle exe args withHandle = 
  runHandles exe args [] $ \_ outH errH -> do
    errVar <- liftIO $ do
      errVar' <- newEmptyMVar
      _ <- forkIO $ transferLinesAndCombine errH stderr >>= putMVar errVar'
      return errVar'

    res <- withHandle outH
    errs <- liftIO $ takeMVar errVar

    modify $ \state' -> state' { sStderr = errs }
    return res

-- | Similar to 'run' but gives direct access to all input and output handles.
--
-- Be careful when using the optional input handles.
-- If you specify Inherit for a handle then attempting to access the handle in your
-- callback is an error
runHandles :: FilePath -- ^ command
           -> [Text] -- ^ arguments
           -> [StdHandle] -- ^ optionally connect process i/o handles to existing handles
           -> (Handle -> Handle -> Handle -> Sh a) -- ^ stdin, stdout and stderr
           -> Sh a
runHandles exe args reusedHandles withHandles = do
    -- clear stdin before beginning command execution
    origstate <- get
    let mStdin = sStdin origstate
    put $ origstate { sStdin = Nothing, sCode = 0, sStderr = T.empty }
    state <- get

    let cmdString = show_command exe args
    when (sPrintCommands state) $ echo cmdString
    trace cmdString

    bracketOnWindowsError
      ((sRun state) reusedHandles state exe args)
      (\(_,_,_,procH) -> (liftIO $ terminateProcess procH))
      (\(inH,outH,errH,procH) -> do
        liftIO $ case mStdin of
          Just input -> TIO.hPutStr inH input
          Nothing -> return ()

        result <- withHandles inH outH errH

        (ex, code) <- liftIO $ do
          ex' <- waitForProcess procH

          -- TODO: specifically catch our own error for Inherit pipes
          hClose outH `catchany` (const $ return ())
          hClose errH `catchany` (const $ return ())
          hClose inH `catchany` (const $ return ())

          return $ case ex' of
            ExitSuccess -> (ex', 0)
            ExitFailure n -> (ex', n)

        modify $ \state' -> state' { sCode = code }

        case (sErrExit state, ex) of
          (True,  ExitFailure n) -> do
              newState <- get
              liftIO $ throwIO $ RunFailed exe args n (sStderr newState)
          _                      -> return result
      )
  where -- Windows does not terminate spawned processes, so we must bracket.
#if defined(mingw32_HOST_OS)
    bracketOnWindowsError acquire release main = do
      resource <- acquire 
      main resource `catchany_sh` (\e -> do
          _ <- release resource
          liftIO $ throwIO e
        )
#else
    bracketOnWindowsError :: Sh a -> (a -> Sh ()) -> (a -> Sh b) -> Sh b
    bracketOnWindowsError acquire _ main = acquire >>= main
#endif


-- | used by 'run'. fold over stdout line-by-line as it is read to avoid keeping it in memory
-- stderr is still being placed in memory under the assumption it is always relatively small
runFoldLines :: a -> FoldCallback a -> FilePath -> [Text] -> Sh a
runFoldLines start cb exe args =
  runHandles exe args [] $ \inH outH errH -> do
    (errVar, outVar) <- liftIO $ do
      hClose inH -- setStdin was taken care of before the process even ran
      errVar' <- newEmptyMVar
      outVar' <- newEmptyMVar
      _ <- forkIO $ transferLinesAndCombine errH stderr >>= putMVar errVar'
      return (errVar', outVar')

    state <- get
    errs <- liftIO $ do
      void $ if sPrintStdout state
        then
          forkIO $ transferFoldHandleLines start cb outH stdout >>= putMVar outVar
        else
          forkIO $ foldHandleLines start cb outH >>= putMVar outVar
      takeMVar errVar

    modify $ \state' -> state' { sStderr = errs }
    liftIO $ takeMVar outVar



-- | The output of last external command. See 'run'.
lastStderr :: Sh Text
lastStderr = gets sStderr

-- | The exit code from the last command.
-- Unless you set 'errExit' to False you won't get a chance to use this: a non-zero exit code will throw an exception.
lastExitCode :: Sh Int
lastExitCode = gets sCode

-- | set the stdin to be used and cleared by the next 'run'.
setStdin :: Text -> Sh ()
setStdin input = modify $ \st -> st { sStdin = Just input }

-- | Pipe operator. set the stdout the first command as the stdin of the second.
-- This does not create a shell-level pipe, but hopefully it will in the future.
-- To create a shell level pipe you can set @escaping False@ and use a pipe @|@ character in a command.
(-|-) :: Sh Text -> Sh b -> Sh b
one -|- two = do
  res <- print_stdout False one
  setStdin res
  two

-- | Copy a file, or a directory recursively.
-- uses 'cp'
cp_r :: FilePath -> FilePath -> Sh ()
cp_r from' to' = do
    from <- absPath from'
    fromIsDir <- (test_d from)
    if not fromIsDir then cp from' to' else do
       to <- absPath to'
       trace $ "cp -r " <> toTextIgnore from <> " " <> toTextIgnore to
       toIsDir <- test_d to

       when (from == to) $ liftIO $ throwIO $ userError $ show $ "cp_r: " <>
         toTextIgnore from <> " and " <> toTextIgnore to <> " are identical"

       finalTo <- if not toIsDir then mkdir to >> return to else do
                   let d = to </> dirname (addTrailingSlash from)
                   mkdir_p d >> return d

       ls from >>= mapM_ (\item -> cp_r (from FP.</> filename item) (finalTo FP.</> filename item))

-- | Copy a file. The second path could be a directory, in which case the
-- original file name is used, in that directory.
cp :: FilePath -> FilePath -> Sh ()
cp from' to' = do
  from <- absPath from'
  to <- absPath to'
  trace $ "cp " <> toTextIgnore from <> " " <> toTextIgnore to
  to_dir <- test_d to
  let to_loc = if to_dir then to FP.</> filename from else to
  liftIO $ copyFile from to_loc `catchany` (\e -> throwIO $
      ReThrownException e (extraMsg to_loc from)
    )
  where
    extraMsg t f = "during copy from: " ++ encodeString f ++ " to: " ++ encodeString t



-- | Create a temporary directory and pass it as a parameter to a Sh
-- computation. The directory is nuked afterwards.
withTmpDir :: (FilePath -> Sh a) -> Sh a
withTmpDir act = do
  trace "withTmpDir"
  dir <- liftIO getTemporaryDirectory
  tid <- liftIO myThreadId
  (pS, handle) <- liftIO $ openTempFile dir ("tmp"++filter isAlphaNum (show tid))
  let p = pack pS
  liftIO $ hClose handle -- required on windows
  rm_f p
  mkdir p
  act p `finally_sh` rm_rf p

-- | Write a Lazy Text to a file.
writefile :: FilePath -> Text -> Sh ()
writefile f' bits = absPath f' >>= \f -> do
  trace $ "writefile " <> toTextIgnore f
  liftIO (TIO.writeFile (encodeString f) bits)

-- | Update a file, creating (a blank file) if it does not exist.
touchfile :: FilePath -> Sh ()
touchfile = absPath >=> flip appendfile ""

-- | Append a Lazy Text to a file.
appendfile :: FilePath -> Text -> Sh ()
appendfile f' bits = absPath f' >>= \f -> do
  trace $ "appendfile " <> toTextIgnore f
  liftIO (TIO.appendFile (encodeString f) bits)

readfile :: FilePath -> Sh Text
readfile = absPath >=> \fp -> do
  trace $ "readfile " <> toTextIgnore fp
  readBinary fp >>=
    return . TE.decodeUtf8With TE.lenientDecode

-- | wraps ByteSting readFile
readBinary :: FilePath -> Sh ByteString
readBinary = absPath >=> liftIO . BS.readFile . encodeString

-- | flipped hasExtension for Text
hasExt :: Text -> FilePath -> Bool
hasExt = flip hasExtension

-- | Run a Sh computation and collect timing  information.
time :: Sh a -> Sh (Double, a)
time what = sub $ do
  trace "time"
  t <- liftIO getCurrentTime
  res <- what
  t' <- liftIO getCurrentTime
  return (realToFrac $ diffUTCTime t' t, res)

-- | threadDelay wrapper that uses seconds
sleep :: Int -> Sh ()
sleep = liftIO . threadDelay . (1000 * 1000 *)
