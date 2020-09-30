{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings,
             FlexibleInstances, IncoherentInstances,
             TypeFamilies, ExistentialQuantification #-}

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
         Sh, ShIO, shelly, shellyNoDir, shellyFailDir, asyncSh, sub
         , silently, verbosely, escaping, print_stdout, print_stderr, print_commands
         , onCommandHandles
         , tracing, errExit
         , log_stdout_with, log_stderr_with

         -- * Running external commands.
         , run, run_, runFoldLines, cmd, FoldCallback
         , bash, bash_, bashPipeFail
         , (-|-), lastStderr, setStdin, lastExitCode
         , command, command_, command1, command1_
         , sshPairs,sshPairsPar, sshPairs_,sshPairsPar_, sshPairsWithOptions
         , sshCommandText, SshMode(..)
         , ShellCmd(..), CmdArg (..)

         -- * Running commands Using handles
         , runHandle, runHandles, transferLinesAndCombine, transferFoldHandleLines
         , StdHandle(..), StdStream(..)

         -- * Handle manipulation
         , HandleInitializer, StdInit(..), initOutputHandles, initAllHandles

         -- * Modifying and querying environment.
         , setenv, get_env, get_env_text, getenv, get_env_def, get_env_all, get_environment, appendToPath, prependToPath

         -- * Environment directory
         , cd, chdir, chdir_p, pwd

         -- * Printing
         , echo, echo_n, echo_err, echo_n_err, inspect, inspect_err
         , tag, trace, show_command

         -- * Querying filesystem.
         , ls, lsT, test_e, test_f, test_d, test_s, test_px, which

         -- * Filename helpers
         , absPath, (</>), (<.>), canonic, canonicalize, relPath, relativeTo, path
         , hasExt

         -- * Manipulating filesystem.
         , mv, rm, rm_f, rm_rf, cp, cp_r, mkdir, mkdir_p, mkdirTree

         -- * reading/writing Files
         , readfile, readBinary, writefile, writeBinary, appendfile, touchfile, withTmpDir

         -- * exiting the program
         , exit, errorExit, quietExit, terror

         -- * Exceptions
         , bracket_sh, catchany, catch_sh, handle_sh, handleany_sh, finally_sh, ShellyHandler(..), catches_sh, catchany_sh
         , ReThrownException(..)
         , RunFailed(..)

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
         , followSymlink
         ) where

import Shelly.Base
import Shelly.Directory
import Shelly.Find
import Control.Monad ( when, unless, void, forM, filterM, liftM2 )
import Control.Monad.Trans ( MonadIO )
import Control.Monad.Reader (ask)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding ( readFile, FilePath, catch)
#else
import Prelude hiding ( readFile, FilePath)
#endif
import Data.Char ( isAlphaNum, isSpace, toLower )
import Data.Typeable
import Data.IORef
import Data.Sequence (Seq, (|>))
import Data.Foldable (toList)
import Data.Maybe
import System.IO ( hClose, stderr, stdout, openTempFile)
import System.IO.Error (isPermissionError, catchIOError, isEOFError, isIllegalOperation)
import System.Exit
import System.Environment
import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async (async, wait, Async)
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

import System.FilePath hiding ((</>), (<.>))
import qualified System.FilePath as FP

import System.Directory ( setPermissions, getPermissions, Permissions(..), getTemporaryDirectory, pathIsSymbolicLink
                        , copyFile, removeFile, doesFileExist, doesDirectoryExist, listDirectory
                        , renameFile, renameDirectory, removeDirectoryRecursive, createDirectoryIfMissing
                        , getCurrentDirectory )
import System.IO (Handle)
import Data.Char (isDigit)

import Data.Tree(Tree(..))
import qualified Data.Set as S
import qualified Data.List as L

{- GHC won't default to Text with this, even with extensions!
 - see: http://hackage.haskell.org/trac/ghc/ticket/6030
class CmdArgs a where
  toTextArgs :: a -> [Text]

instance CmdArgs Text       where toTextArgs t = [t]
instance CmdArgs FilePath   where toTextArgs t = [toTextIgnore t]
instance CmdArgs [Text]     where toTextArgs = id
instance CmdArgs [FilePath] where toTextArgs = map toTextIgnore

instance CmdArgs (Text, Text) where
  toTextArgs (t1,t2) = [t1, t2]
instance CmdArgs (FilePath, FilePath) where
  toTextArgs (fp1,fp2) = [toTextIgnore fp1, toTextIgnore fp2]
instance CmdArgs (Text, FilePath) where
  toTextArgs (t1, fp1) = [t1, toTextIgnore fp1]
instance CmdArgs (FilePath, Text) where
  toTextArgs (fp1,t1) = [toTextIgnore fp1, t1]

cmd :: (CmdArgs args) => FilePath -> args -> Sh Text
cmd fp args = run fp $ toTextArgs args
-}

-- | Argument converter for the variadic argument version of 'run' called 'cmd'.
-- Useful for a type signature of a function that uses 'cmd'
class CmdArg a where toTextArg :: a -> Text
instance CmdArg Text     where toTextArg = id
instance CmdArg String   where toTextArg = T.pack

-- | For the variadic function 'cmd'
--
-- partially applied variadic functions require type signatures
class ShellCmd t where
    cmdAll :: FilePath -> [Text] -> t

instance ShellCmd (Sh Text) where
    cmdAll = run

instance (s ~ Text, Show s) => ShellCmd (Sh s) where
    cmdAll = run

-- note that Sh () actually doesn't work for its case (_<- cmd) when there is no type signature
instance ShellCmd (Sh ()) where
    cmdAll = run_

instance (CmdArg arg, ShellCmd result) => ShellCmd (arg -> result) where
    cmdAll fp acc x = cmdAll fp (acc ++ [toTextArg x])

instance (CmdArg arg, ShellCmd result) => ShellCmd ([arg] -> result) where
    cmdAll fp acc x = cmdAll fp (acc ++ map toTextArg x)



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
cmd :: (ShellCmd result) => FilePath -> result
cmd fp = cmdAll fp []

-- | Convert Text to a FilePath-
fromText :: Text -> FilePath
fromText = T.unpack

-- | Helper to convert a Text to a FilePath. Used by '(</>)' and '(<.>)'
class ToFilePath a where
  toFilePath :: a -> FilePath

instance ToFilePath FilePath where toFilePath = id
instance ToFilePath Text     where toFilePath = T.unpack


-- | uses System.FilePath, but can automatically convert a Text
(</>) :: (ToFilePath filepath1, ToFilePath filepath2) => filepath1 -> filepath2 -> FilePath
x </> y = toFilePath x FP.</> toFilePath y

-- | uses System.FilePath, but can automatically convert a Text
(<.>) :: (ToFilePath filepath) => filepath -> Text -> FilePath
x <.> y = toFilePath x FP.<.> T.unpack y


toTextWarn :: FilePath -> Sh Text
toTextWarn efile = do
  when (not $ isValid efile) $ encodeError (T.pack $ makeValid efile)
  return (T.pack $ makeValid efile)
  where
    encodeError f = echo ("non-unicode file name: " <> f)

-- | Transfer from one handle to another
-- For example, send contents of a process output to stdout.
-- does not close the write handle.
--
-- Also, return the complete contents being streamed line by line.
transferLinesAndCombine :: Handle -> (Text -> IO ()) -> IO Text
transferLinesAndCombine readHandle putWrite =
  transferFoldHandleLines mempty (|>) readHandle putWrite >>=
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
transferFoldHandleLines :: a -> FoldCallback a -> Handle -> (Text -> IO ()) -> IO a
transferFoldHandleLines start foldLine readHandle putWrite = go start
  where
    go acc = do
        mLine <- filterIOErrors $ TIO.hGetLine readHandle
        case mLine of
            Nothing -> return acc
            Just line -> putWrite line >> go (foldLine acc line)

filterIOErrors :: IO a -> IO (Maybe a)
filterIOErrors action = catchIOError
               (fmap Just action)
               (\e -> if isEOFError e || isIllegalOperation e -- handle was closed
                       then return Nothing
                       else ioError e)

foldHandleLines :: a -> FoldCallback a -> Handle -> IO a
foldHandleLines start foldLine readHandle = go start
  where
    go acc = do
      mLine <- filterIOErrors $ TIO.hGetLine readHandle
      case mLine of
        Nothing -> return acc
        Just line -> go $ foldLine acc line

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
    RawCommand fullExe (map T.unpack args)
  where
    findExe :: FilePath -> Sh FilePath
    findExe
#if defined(mingw32_HOST_OS) || (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 708)
      fp
#else
      _fp
#endif
      = do
        mExe <- whichEith exe
        case mExe of
          Right execFp -> return execFp
          -- windows looks in extra places besides the PATH, so just give
          -- up even if the behavior is not properly specified anymore
          --
          -- non-Windows < 7.8 has a bug for read-only file systems
          -- https://github.com/yesodweb/Shelly.hs/issues/56
          -- it would be better to specifically detect that bug
#if defined(mingw32_HOST_OS) || (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 708)
          Left _ -> return fp
#else
          Left err -> liftIO $ throwIO $ userError err
#endif




shellyProcess :: [StdHandle] -> State -> CmdSpec -> IO (Handle, Handle, Handle, ProcessHandle)
shellyProcess reusedHandles st cmdSpec =  do
    (createdInH, createdOutH, createdErrorH, pHandle) <- createProcess CreateProcess {
          cmdspec = cmdSpec
        , cwd = Just $ sDirectory st
        , env = Just $ sEnvironment st
        , std_in  = createUnless mInH
        , std_out = createUnless mOutH
        , std_err = createUnless mErrorH
        , close_fds = False
#if MIN_VERSION_process(1,1,0)
        , create_group = False
#endif
#if MIN_VERSION_process(1,2,0)
        , delegate_ctlc = False
#endif
#if MIN_VERSION_process(1,3,0)
        , detach_console = False
        , create_new_console = False
        , new_session = False
#endif
#if MIN_VERSION_process(1,4,0)
        , child_group = Nothing
        , child_user = Nothing
#endif
#if MIN_VERSION_process(1,5,0)
        , use_process_jobs = False
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
    toHandle (Just CreatePipe)    = error "shelly process creation failure CreatePipe"
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
catch_sh action handler = do
    ref <- ask
    liftIO $ catch (runSh action ref) (\e -> runSh (handler e) ref)

-- | Same as a normal 'handle' but specialized for the Sh monad.
handle_sh :: (Exception e) => (e -> Sh a) -> Sh a -> Sh a
handle_sh handler action = do
    ref <- ask
    liftIO $ handle (\e -> runSh (handler e) ref) (runSh action ref)


-- | Same as a normal 'finally' but specialized for the 'Sh' monad.
finally_sh :: Sh a -> Sh b -> Sh a
finally_sh action handler = do
    ref <- ask
    liftIO $ finally (runSh action ref) (runSh handler ref)

bracket_sh :: Sh a -> (a -> Sh b) -> (a -> Sh c) -> Sh c
bracket_sh acquire release main = do
  ref <- ask
  liftIO $ bracket (runSh acquire ref)
                   (\resource -> runSh (release resource) ref)
                   (\resource -> runSh (main resource) ref)



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
    toHandler runner (ShellyHandler handler) = Handler (\e -> runner (handler e))

-- | Catch any exception in the Sh monad.
catchany_sh :: Sh a -> (SomeException -> Sh a) -> Sh a
catchany_sh = catch_sh

-- | Handle any exception in the Sh monad.
handleany_sh :: (SomeException -> Sh a) -> Sh a -> Sh a
handleany_sh = handle_sh

-- | Change current working directory of Sh. This does *not* change the
-- working directory of the process we are running it. Instead, Sh keeps
-- track of its own working directory and builds absolute paths internally
-- instead of passing down relative paths.
cd :: FilePath -> Sh ()
cd = traceCanonicPath ("cd " <>) >=> cd'
  where
    cd' dir = do
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
pack = id

-- | Move a file. The second path could be a directory, in which case the
-- original file is moved into that directory.
-- wraps directory 'System.Directory.renameFile', which may not work across FS boundaries
mv :: FilePath -> FilePath -> Sh ()
mv from' to' = do
  trace $ "mv " <> toTextIgnore from' <> " " <> toTextIgnore to'
  from <- absPath from'
  from_dir <- test_d from
  to <- absPath to'
  to_dir <- test_d to
  let to_loc = if not to_dir then to else to FP.</> (FP.takeFileName from)
  liftIO $ createDirectoryIfMissing True (takeDirectory to_loc)
  if not from_dir
    then liftIO $ renameFile from to_loc
      `catchany` (\e -> throwIO $
        ReThrownException e (extraMsg to_loc from)
      )
    else liftIO $ renameDirectory from to_loc
      `catchany` (\e -> throwIO $
        ReThrownException e (extraMsg to_loc from)
      )
  where
    extraMsg :: String -> String -> String
    extraMsg t f = "during copy from: " ++ f ++ " to: " ++ t

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
mkdir = traceAbsPath ("mkdir " <>) >=>
        liftIO . createDirectoryIfMissing False

-- | Create a new directory, including parents (succeeds if the directory
-- already exists).
mkdir_p :: FilePath -> Sh ()
mkdir_p = traceAbsPath ("mkdir -p " <>) >=>
          liftIO . createDirectoryIfMissing True

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


isExecutable :: FilePath -> IO Bool
isExecutable f = (executable `fmap` getPermissions f) `catch` (\(_ :: IOError) -> return False)

-- | Get a full path to an executable by looking at the @PATH@ environement
-- variable. Windows normally looks in additional places besides the
-- @PATH@: this does not duplicate that behavior.
which :: FilePath -> Sh (Maybe FilePath)
which fp = either (const Nothing) Just <$> whichEith fp

-- | Get a full path to an executable by looking at the @PATH@ environement
-- variable. Windows normally looks in additional places besides the
-- @PATH@: this does not duplicate that behavior.
whichEith :: FilePath -> Sh (Either String FilePath)
whichEith originalFp = whichFull
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
        whichUntraced | isAbsolute fp          = checkFile
                      | dotSlash splitOnDirs   = checkFile
                      | length splitOnDirs > 0 = lookupPath  >>= leftPathError
                      | otherwise              = lookupCache >>= leftPathError

        splitOnDirs = splitDirectories fp
        dotSlash ("./":_) = True
        dotSlash _ = False

        checkFile :: Sh (Either String FilePath)
        checkFile = do
            exists <- liftIO $ doesFileExist fp
            return $ if exists then Right fp else
              Left $ "did not find file: " <> fp

        leftPathError :: Maybe FilePath -> Sh (Either String FilePath)
        leftPathError Nothing  = Left <$> pathLookupError
        leftPathError (Just x) = return $ Right x

        pathLookupError :: Sh String
        pathLookupError = do
          pATH <- get_env_text "PATH"
          return $
            "shelly did not find " `mappend` fp `mappend`
            " in the PATH: " `mappend` T.unpack pATH

        lookupPath :: Sh (Maybe FilePath)
        lookupPath = (pathDirs >>=) $ findMapM $ \dir -> do
            let fullFp = dir </> fp
            res <- liftIO $ isExecutable fullFp
            return $ if res then Just fullFp else Nothing

        lookupCache :: Sh (Maybe FilePath)
        lookupCache = do
            pathExecutables <- cachedPathExecutables
            return $ fmap (flip (</>) fp . fst) $
                L.find (S.member fp . snd) pathExecutables


        pathDirs = mapM absPath =<< ((map T.unpack . filter (not . T.null) . T.split (== searchPathSeparator)) `fmap` get_env_text "PATH")

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
                      map (\f -> (f, takeFileName f)) files
                    return $ S.fromList exes
                  )
                let cachedExecutables = zip dirs executables
                modify $ \x -> x { sPathExecutables = Just cachedExecutables }
                return $ cachedExecutables


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
    file <- doesFileExist f
    if file then return True else doesDirectoryExist f

-- | Does a path point to an existing file?
test_f :: FilePath -> Sh Bool
test_f = absPath >=> liftIO . doesFileExist

-- | Test that a file is in the PATH and also executable
test_px :: FilePath -> Sh Bool
test_px exe = do
  mFull <- which exe
  case mFull of
    Nothing -> return False
    Just full -> liftIO $ isExecutable full

-- | A swiss army cannon for removing things. Actually this goes farther than a
-- normal rm -rf, as it will circumvent permission problems for the files we
-- own. Use carefully.
-- Uses 'removeDirectoryRecursive'
rm_rf :: FilePath -> Sh ()
rm_rf infp = do
  f <- traceAbsPath ("rm -rf " <>) infp
  isDir <- (test_d f)
  if not isDir then whenM (test_f f) $ rm_f f
    else
      (liftIO_ $ removeDirectoryRecursive f) `catch_sh` (\(e :: IOError) ->
        when (isPermissionError e) $ do
          find f >>= mapM_ (\file -> liftIO_ $ fixPermissions file `catchany` \_ -> return ())
          liftIO $ removeDirectoryRecursive f
        )
  where fixPermissions file =
          do permissions <- liftIO $ getPermissions file
             let deletable = permissions { readable = True, writable = True, executable = True }
             liftIO $ setPermissions file deletable

-- | Remove a file. Does not fail if the file does not exist.
-- Does fail if the file is not a file.
rm_f :: FilePath -> Sh ()
rm_f = traceAbsPath ("rm -f " <>) >=> \f ->
  whenM (test_e f) $ liftIO $ removeFile f

-- | Remove a file.
-- Does fail if the file does not exist (use 'rm_f' instead) or is not a file.
rm :: FilePath -> Sh ()
rm = traceAbsPath ("rm " <>) >=>
  -- TODO: better error message for removeFile (give takeFileName)
  liftIO . removeFile

-- | Set an environment variable. The environment is maintained in Sh
-- internally, and is passed to any external commands to be executed.
setenv :: Text -> Text -> Sh ()
setenv k v = if k == path_env then setPath v else setenvRaw k v

setenvRaw :: Text -> Text -> Sh ()
setenvRaw k v = modify $ \x -> x { sEnvironment = wibble $ sEnvironment x }
  where
    normK = normalizeEnvVarNameText k
    (kStr, vStr) = (T.unpack normK, T.unpack v)
    wibble environment = (kStr, vStr) : filter ((/=kStr) . fst) environment

setPath :: Text -> Sh ()
setPath newPath = do
  modify $ \x -> x{ sPathExecutables = Nothing }
  setenvRaw path_env newPath

path_env :: Text
path_env = normalizeEnvVarNameText "PATH"

-- | add the filepath onto the PATH env variable
appendToPath :: FilePath -> Sh ()
appendToPath = traceAbsPath ("appendToPath: " <>) >=> \filepath -> do
  tp <- toTextWarn filepath
  pe <- get_env_text path_env
  setPath $ pe <> T.singleton searchPathSeparator <> tp

-- | prepend the filepath to the PATH env variable
-- similar to 'appendToPath' but gives high priority to the filepath instead of low priority.
prependToPath :: FilePath -> Sh ()
prependToPath = traceAbsPath ("prependToPath: " <>) >=> \filepath -> do
  tp <- toTextWarn filepath
  pe <- get_env_text path_env
  setPath $ tp <> T.singleton searchPathSeparator <> pe

get_environment :: Sh [(String, String)]
get_environment = gets sEnvironment
{-# DEPRECATED get_environment "use get_env_all" #-}

-- | get the full environment
get_env_all :: Sh [(String, String)]
get_env_all = gets sEnvironment

-- On Windows, normalize all environment variable names (to lowercase)
-- to account for case insensitivity.
#if defined(mingw32_HOST_OS)

normalizeEnvVarNameText :: Text -> Text
normalizeEnvVarNameText = T.toLower

normalizeEnvVarNameString :: String -> String
normalizeEnvVarNameString = fmap toLower

-- On other systems, keep the variable names as-is.
#else

normalizeEnvVarNameText :: Text -> Text
normalizeEnvVarNameText = id

normalizeEnvVarNameString :: String -> String
normalizeEnvVarNameString = id

#endif

-- | Fetch the current value of an environment variable.
-- if non-existant or empty text, will be Nothing
get_env :: Text -> Sh (Maybe Text)
get_env k = do
  mval <- return
          . fmap T.pack
          . lookup (T.unpack normK)
          =<< gets sEnvironment
  return $ case mval of
    Nothing  -> Nothing
    Just val -> if (not $ T.null val) then Just val else Nothing
  where
  normK = normalizeEnvVarNameText k

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

-- | Apply a single initializer to the two output process handles (stdout and stderr)
initOutputHandles :: HandleInitializer -> StdInit
initOutputHandles f = StdInit (const $ return ()) f f

-- | Apply a single initializer to all three standard process handles (stdin, stdout and stderr)
initAllHandles :: HandleInitializer -> StdInit
initAllHandles f = StdInit f f f

-- | When running an external command, apply the given initializers to
-- the specified handles for that command.
-- This can for example be used to change the encoding of the
-- handles or set them into binary mode.
onCommandHandles :: StdInit -> Sh a -> Sh a
onCommandHandles initHandles a =
    sub $ modify (\x -> x { sInitCommandHandles = initHandles }) >> a

-- | Create a sub-Sh in which external command outputs are not echoed and
-- commands are not printed.
-- See 'sub'.
silently :: Sh a -> Sh a
silently a = sub $ modify (\x -> x
                                { sPrintStdout = False
                                , sPrintStderr = False
                                , sPrintCommands = False
                                }) >> a

-- | Create a sub-Sh in which external command outputs are echoed and
-- Executed commands are printed
-- See 'sub'.
verbosely :: Sh a -> Sh a
verbosely a = sub $ modify (\x -> x
                                 { sPrintStdout = True
                                 , sPrintStderr = True
                                 , sPrintCommands = True
                                 }) >> a

-- | Create a sub-Sh in which stdout is sent to the user-defined
-- logger.  When running with 'silently' the given log will not be
-- called for any output. Likewise the log will also not be called for
-- output from 'run_' and 'bash_' commands.
log_stdout_with :: (Text -> IO ()) -> Sh a -> Sh a
log_stdout_with logger a = sub $ modify (\s -> s { sPutStdout = logger })
                                 >> a

-- | Create a sub-Sh in which stderr is sent to the user-defined
-- logger.  When running with 'silently' the given log will not be
-- called for any output. However, unlike 'log_stdout_with' the log
-- will be called for output from 'run_' and 'bash_' commands.
log_stderr_with :: (Text -> IO ()) -> Sh a -> Sh a
log_stderr_with logger a = sub $ modify (\s -> s { sPutStderr = logger })
                                 >> a

-- | Create a sub-Sh with stdout printing on or off
-- Defaults to True.
print_stdout :: Bool -> Sh a -> Sh a
print_stdout shouldPrint a =
  sub $ modify (\x -> x { sPrintStdout = shouldPrint }) >> a

-- | Create a sub-Sh with stderr printing on or off
-- Defaults to True.
print_stderr :: Bool -> Sh a -> Sh a
print_stderr shouldPrint a =
  sub $ modify (\x -> x { sPrintStderr = shouldPrint }) >> a


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
  modify $ \st -> st { sCommandEscaping = shouldEscape }
  action

-- | named after bash -e errexit. Defaults to @True@.
-- When @True@, throw an exception on a non-zero exit code.
-- When @False@, ignore a non-zero exit code.
-- Not recommended to set to @False@ unless you are specifically checking the error code with 'lastExitCode'.
errExit :: Bool -> Sh a -> Sh a
errExit shouldExit action = sub $ do
  modify $ \st -> st { sErrExit = shouldExit }
  action

-- | 'find'-command follows symbolic links. Defaults to @False@.
-- When @True@, follow symbolic links.
-- When @False@, never follow symbolic links.
followSymlink :: Bool -> Sh a -> Sh a
followSymlink enableFollowSymlink action = sub $ do
  modify $ \st -> st { sFollowSymlink = enableFollowSymlink }
  action


defReadOnlyState :: ReadOnlyState
defReadOnlyState = ReadOnlyState { rosFailToDir = False }

-- | Deprecated now, just use 'shelly', whose default has been changed.
-- Using this entry point does not create a @.shelly@ directory in the case
-- of failure. Instead it logs directly into the standard error stream (@stderr@).
shellyNoDir :: MonadIO m => Sh a -> m a
shellyNoDir = shelly' ReadOnlyState { rosFailToDir = False }
{-# DEPRECATED shellyNoDir "Just use shelly. The default settings have changed" #-}

-- | Using this entry point creates a @.shelly@ directory in the case
-- of failure where errors are recorded.
shellyFailDir :: MonadIO m => Sh a -> m a
shellyFailDir = shelly' ReadOnlyState { rosFailToDir = True }

getNormalizedEnvironment :: IO [(String, String)]
getNormalizedEnvironment =
#if defined(mingw32_HOST_OS)
  -- On Windows, normalize all environment variable names (to lowercase)
  -- to account for case insensitivity.
  fmap (\(a, b) -> (normalizeEnvVarNameString a, b)) <$> getEnvironment
#else
  -- On other systems, keep the environment as-is.
  getEnvironment
#endif

-- | Enter a Sh from (Monad)IO. The environment and working directories are
-- inherited from the current process-wide values. Any subsequent changes in
-- processwide working directory or environment are not reflected in the
-- running Sh.
shelly :: MonadIO m => Sh a -> m a
shelly = shelly' defReadOnlyState

shelly' :: MonadIO m => ReadOnlyState -> Sh a -> m a
shelly' ros action = do
  environment <- liftIO getNormalizedEnvironment
  dir <- liftIO getCurrentDirectory
  let def  = State { sCode = 0
                   , sStdin = Nothing
                   , sStderr = T.empty
                   , sPutStdout = TIO.hPutStrLn stdout
                   , sPutStderr = TIO.hPutStrLn stderr
                   , sPrintStdout = True
                   , sPrintStderr = True
                   , sPrintCommands = False
                   , sInitCommandHandles = initAllHandles (const $ return ())
                   , sCommandEscaping = True
                   , sEnvironment = environment
                   , sTracing = True
                   , sTrace = T.empty
                   , sDirectory = dir
                   , sPathExecutables = Nothing
                   , sErrExit = True
                   , sReadOnly = ros
                   , sFollowSymlink = False
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
      if not (rosFailToDir $ sReadOnly st) then ranCommands else do
          d <- pwd
          sf <- shellyFile
          let logFile = d</>shelly_dir</>sf
          (writefile logFile trc >> return ("log of commands saved to: " <> logFile))
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
    nextNum fs = (+ 1) . maximum . map (readDef 1 . filter isDigit . takeFileName) $ fs

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

-- quote one argument
quoteOne :: Text -> Text
quoteOne t =
    surround '\'' $ T.replace "'" "'\\''" t


-- returns a string that can be executed by a shell.
-- NOTE: all parts are treated literally, which means that
-- things like variable expansion will not be available.
quoteCommand :: FilePath -> [Text] -> Text
quoteCommand exe args =
    T.intercalate " " $ map quoteOne (toTextIgnore exe : args)

surround :: Char -> Text -> Text
surround c t = T.cons c $ T.snoc t c

data SshMode = ParSsh | SeqSsh

-- | same as 'sshPairs', but returns ()
sshPairs_ :: Text -> [(FilePath, [Text])] -> Sh ()
sshPairs_ _ [] = return ()
sshPairs_ server cmds = sshPairs' run_ server cmds

-- | same as 'sshPairsP', but returns ()

sshPairsPar_ :: Text -> [(FilePath, [Text])] -> Sh ()
sshPairsPar_ _ [] = return ()
sshPairsPar_ server cmds = sshPairsPar' run_ server cmds

-- | run commands over SSH.
-- An ssh executable is expected in your path.
-- Commands are in the same form as 'run', but given as pairs
--
-- > sshPairs "server-name" [("cd", "dir"), ("rm",["-r","dir2"])]
--
-- This interface is crude, but it works for now.
--
-- Please note this sets 'escaping' to False, and the remote commands are
-- quoted with single quotes, in a way such that the remote commands will see
-- the literal values you passed, this means that no variable expansion and
-- alike will done on either the local shell or the remote shell, and that
-- if there are a single or double quotes in your arguments, they need not
-- to be quoted manually.
--
-- Internally the list of commands are combined with the string @&&@ before given to ssh.
sshPairs :: Text -> [(FilePath, [Text])] -> Sh Text
sshPairs _ [] = return ""
sshPairs server cmds = sshPairsWithOptions' run server [] cmds SeqSsh

-- | Same as sshPairs, but combines commands with the string @&@, so they will be started in parallell.
sshPairsPar :: Text -> [(FilePath, [Text])] -> Sh Text
sshPairsPar _ [] = return ""
sshPairsPar server cmds = sshPairsWithOptions' run server [] cmds ParSsh

sshPairsPar' :: (FilePath -> [Text] -> Sh a) -> Text -> [(FilePath, [Text])] -> Sh a
sshPairsPar' run' server actions = sshPairsWithOptions' run' server [] actions ParSsh

sshPairs' :: (FilePath -> [Text] -> Sh a) -> Text -> [(FilePath, [Text])] -> Sh a
sshPairs' run' server actions = sshPairsWithOptions' run' server [] actions SeqSsh

-- | Like 'sshPairs', but allows for arguments to the call to ssh.
sshPairsWithOptions :: Text                  -- ^ Server name.
                    -> [Text]                -- ^ Arguments to ssh (e.g. ["-p","22"]).
                    -> [(FilePath, [Text])]  -- ^ Pairs of commands to run on the remote.
                    -> Sh Text               -- ^ Returns the standard output.
sshPairsWithOptions _ _ [] = return ""
sshPairsWithOptions server sshargs cmds = sshPairsWithOptions' run server sshargs cmds SeqSsh

sshPairsWithOptions' :: (FilePath -> [Text] -> Sh a) -> Text -> [Text] -> [(FilePath, [Text])] -> SshMode  -> Sh a
sshPairsWithOptions' run' server sshargs actions mode = escaping False $ do
    run' "ssh" ([server] ++ sshargs ++ [sshCommandText actions mode])

sshCommandText :: [(FilePath, [Text])] -> SshMode -> Text
sshCommandText actions mode =
    quoteOne (foldl1 joiner (map (uncurry quoteCommand) actions))
  where
    joiner memo next = case mode of
        SeqSsh -> memo <> " && " <> next
        ParSsh -> memo <> " & " <> next

data QuietExit = QuietExit Int deriving (Show, Typeable)
instance Exception QuietExit

-- | Shelly's wrapper around exceptions thrown in its monad
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

-- | Like 'run', but it invokes the user-requested program with _bash_.
bash :: FilePath -> [Text] -> Sh Text
bash fp args = escaping False $ run "bash" $ bashArgs fp args

bash_ :: FilePath -> [Text] -> Sh ()
bash_ fp args = escaping False $ run_ "bash" $ bashArgs fp args

bashArgs :: FilePath -> [Text] -> [Text]
bashArgs fp args = ["-c", "'" <> sanitise (toTextIgnore fp : args) <> "'"]
  where
    sanitise = T.replace "'" "\'" . T.intercalate " "

-- | Use this with 'bash' to set _pipefail_
--
-- > bashPipeFail $ bash "echo foo | echo"
bashPipeFail :: (FilePath -> [Text] -> Sh a) -> FilePath -> [Text] -> Sh a
bashPipeFail runner fp args = runner "set -o pipefail;" (toTextIgnore fp : args)

-- | bind some arguments to 'run' for re-use. Example:
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

-- | bind some arguments to 'run' for re-use, and require 1 argument. Example:
--
-- > git = command1 "git" []
-- > git "pull" ["origin", "master"]
command1 :: FilePath -> [Text] -> Text -> [Text] -> Sh Text
command1 com args one_arg more_args = run com (args ++ [one_arg] ++ more_args)

-- | bind some arguments to 'run_' for re-use, and require 1 argument. Example:
--
-- > git_ = command1_ "git" []
-- > git "pull" ["origin", "master"]
command1_ :: FilePath -> [Text] -> Text -> [Text] -> Sh ()
command1_ com args one_arg more_args = run_ com (args ++ [one_arg] ++ more_args)

-- | the same as 'run', but return @()@ instead of the stdout content
-- stdout will be read and discarded line-by-line
run_ :: FilePath -> [Text] -> Sh ()
run_ exe args = do
    state <- get
    if sPrintStdout state
      then runWithColor_
      else runFoldLines () (\_ _ -> ()) exe args
  where
    -- same a runFoldLines except Inherit Stdout
    -- That allows color to show up
    runWithColor_ =
        runHandles exe args [OutHandle Inherit] $ \inH _ errH -> do
          state <- get
          errs <- liftIO $ do
            hClose inH -- setStdin was taken care of before the process even ran
            errVar <- (putHandleIntoMVar mempty (|>) errH (sPutStderr state) (sPrintStderr state))
            lineSeqToText `fmap` wait errVar
          modify $ \state' -> state' { sStderr = errs }
          return ()

liftIO_ :: IO a -> Sh ()
liftIO_ = void . liftIO

-- | Similar to 'run' but gives the raw stdout handle in a callback.
-- If you want even more control, use 'runHandles'.
runHandle :: FilePath -- ^ command
          -> [Text] -- ^ arguments
          -> (Handle -> Sh a) -- ^ stdout handle
          -> Sh a
runHandle exe args withHandle = runHandles exe args [] $ \_ outH errH -> do
    state <- get
    errVar <- liftIO $
      (putHandleIntoMVar mempty (|>) errH (sPutStderr state) (sPrintStderr state))
    res <- withHandle outH
    errs <- liftIO $ lineSeqToText `fmap` wait errVar
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

    let doRun = if sCommandEscaping state then runCommand else runCommandNoEscape

    bracket_sh
      (doRun reusedHandles state exe args)
      (\(_,_,_,procH) -> (liftIO $ terminateProcess procH))
      (\(inH,outH,errH,procH) -> do

        liftIO $ do
          inInit (sInitCommandHandles state) inH
          outInit (sInitCommandHandles state) outH
          errInit (sInitCommandHandles state) errH

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


-- | used by 'run'. fold over stdout line-by-line as it is read to avoid keeping it in memory
-- stderr is still being placed in memory under the assumption it is always relatively small
runFoldLines :: a -> FoldCallback a -> FilePath -> [Text] -> Sh a
runFoldLines start cb exe args =
  runHandles exe args [] $ \inH outH errH -> do
    state <- get
    (errVar, outVar) <- liftIO $ do
      hClose inH -- setStdin was taken care of before the process even ran
      liftM2 (,)
          (putHandleIntoMVar mempty (|>) errH (sPutStderr state) (sPrintStderr state))
          (putHandleIntoMVar start cb outH (sPutStdout state) (sPrintStdout state))
    errs <- liftIO $ lineSeqToText `fmap` wait errVar
    modify $ \state' -> state' { sStderr = errs }
    liftIO $ wait outVar


putHandleIntoMVar :: a -> FoldCallback a
                  -> Handle -- ^ out handle
                  -> (Text -> IO ()) -- ^ in handle
                  -> Bool  -- ^ should it be printed while transfered?
                  -> IO (Async a)
putHandleIntoMVar start cb outH putWrite shouldPrint = liftIO $ async $ do
  if shouldPrint
    then transferFoldHandleLines start cb outH putWrite
    else foldHandleLines start cb outH


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
    if not fromIsDir then cp_should_follow_symlinks False from' to' else do
       trace $ "cp_r " <> toTextIgnore from <> " " <> toTextIgnore to'
       to <- absPath to'
       toIsDir <- test_d to

       when (from == to) $ liftIO $ throwIO $ userError $ show $ "cp_r: " <>
         toTextIgnore from <> " and " <> toTextIgnore to <> " are identical"

       finalTo <- if not toIsDir then do
            mkdir to
            return to
          else do
            -- this takes the name of the from directory
            -- because filepath has no builtin function like `dirname`
            let d = to </> (last . splitPath $ takeDirectory (addTrailingPathSeparator from))
            mkdir_p d >> return d
       ls from >>= mapM_ (\item -> do
         cp_r (from FP.</> takeFileName item) (finalTo FP.</> takeFileName item))

-- | Copy a file. The second path could be a directory, in which case the
-- original file name is used, in that directory.
cp :: FilePath -> FilePath -> Sh ()
cp = cp_should_follow_symlinks True

cp_should_follow_symlinks :: Bool -> FilePath -> FilePath -> Sh ()
cp_should_follow_symlinks shouldFollowSymlinks from' to' = do
  from <- absPath from'
  to <- absPath to'
  trace $ "cp " <> toTextIgnore from <> " " <> toTextIgnore to
  to_dir <- test_d to
  let to_loc = if to_dir then to FP.</> takeFileName from else to
  if shouldFollowSymlinks then copyNormal from to_loc else do
    isSymlink <- liftIO $ pathIsSymbolicLink from
    if not isSymlink then copyNormal from to_loc else do
      target <- liftIO $ getSymbolicLinkTarget from
      liftIO $ createFileLink target to_loc
  where
    extraMsg :: String -> String -> String
    extraMsg t f = "during copy from: " ++ f ++ " to: " ++ t
    copyNormal from to = liftIO $ copyFile from to `catchany` (\e -> throwIO $
          ReThrownException e (extraMsg to from)
        )

-- | Create a temporary directory and pass it as a parameter to a Sh
-- computation. The directory is nuked afterwards.
withTmpDir :: (FilePath -> Sh a) -> Sh a
withTmpDir act = do
  trace "withTmpDir"
  dir <- liftIO getTemporaryDirectory
  tid <- liftIO myThreadId
  (pS, fhandle) <- liftIO $ openTempFile dir ("tmp" ++ filter isAlphaNum (show tid))
  let p = pack pS
  liftIO $ hClose fhandle -- required on windows
  rm_f p
  mkdir p
  act p `finally_sh` rm_rf p

-- | Write a Text to a file.
writefile :: FilePath -> Text -> Sh ()
writefile f' bits = do
  f <- traceAbsPath ("writefile " <>) f'
  liftIO (TIO.writeFile f bits)

writeBinary :: FilePath -> ByteString -> Sh ()
writeBinary f' bytes = do
  f <- traceAbsPath ("writeBinary " <>) f'
  liftIO (BS.writeFile f bytes)

-- | Update a file, creating (a blank file) if it does not exist.
touchfile :: FilePath -> Sh ()
touchfile = traceAbsPath ("touch " <>) >=> flip appendfile ""

-- | Append a Text to a file.
appendfile :: FilePath -> Text -> Sh ()
appendfile f' bits = do
  f <- traceAbsPath ("appendfile " <>) f'
  liftIO (TIO.appendFile f bits)

readfile :: FilePath -> Sh Text
readfile = traceAbsPath ("readfile " <>) >=> \fp ->
  readBinary fp >>=
    return . TE.decodeUtf8With TE.lenientDecode

-- | wraps ByteSting readFile
readBinary :: FilePath -> Sh ByteString
readBinary = traceAbsPath ("readBinary " <>)
         >=> liftIO . BS.readFile

-- | flipped hasExtension for Text
hasExt :: Text -> FilePath -> Bool
hasExt ext fp = T.pack (FP.takeExtension fp) == ext

-- | Run a Sh computation and collect timing information.
--   The value returned is the amount of _real_ time spent running the computation
--   in seconds, as measured by the system clock.
--   The precision is determined by the resolution of `getCurrentTime`.
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

-- | spawn an asynchronous action with a copy of the current state
asyncSh :: Sh a -> Sh (Async a)
asyncSh proc = do
  state <- get
  liftIO $ async $ shelly (put state >> proc)

-- helper because absPath can throw exceptions
-- This helps give clear tracing messages
tracePath :: (FilePath -> Sh FilePath) -- ^ filepath conversion
          -> (Text -> Text) -- ^ tracing statement
          -> FilePath
          -> Sh FilePath -- ^ converted filepath
tracePath convert tracer infp =
  (convert infp >>= \fp -> traceIt fp >> return fp)
  `catchany_sh` (\e -> traceIt infp >> liftIO (throwIO e))
    where traceIt = trace . tracer . toTextIgnore

traceAbsPath :: (Text -> Text) -> FilePath -> Sh FilePath
traceAbsPath = tracePath absPath

traceCanonicPath :: (Text -> Text) -> FilePath -> Sh FilePath
traceCanonicPath = tracePath canonic
