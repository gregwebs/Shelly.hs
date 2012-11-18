{-# LANGUAGE 
        FlexibleInstances, 
        TypeSynonymInstances, 
        TypeFamilies,
        ExistentialQuantification #-}
-- | This module is a wrapper for the module "Shelly". 
-- The only difference is a main type "Sh". In this module 
-- "Sh" contains a list of results. Actual definition of the type "Sh" is:
--
-- > import qualified Shelly as S
-- >
-- > newtype Sh a = Sh { unSh :: S.Sh [a] }
--
-- This definition can simplify some filesystem commands. 
-- A monad bind operator becomes a pipe operator and we can write
--
-- > findExt ext = findWhen (pure . hasExt ext)
-- >
-- > main :: IO ()
-- > main = shs $ do
-- >     mkdir "new"
-- >     findExt "hs"  "." >>= flip cp "new"
-- >     findExt "cpp" "." >>= rm_f 
-- >     liftIO $ putStrLn "done"
--
-- Monad methods "return" and ">>=" behave like methods for
-- @ListT Shelly.Sh@, but ">>" forgets the number of 
-- the empty effects. So the last line prints @\"done\"@ only once. 
--
-- I highly recommend putting the following at the top of your program,
-- otherwise you will likely need either type annotations or type conversions
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- > import Shelly
-- > import Data.Text.Lazy as LT
-- > default (LT.Text)
module Shelly.Pipe
       (
         -- * Entering Sh.
         Sh, shs, shelly,shellyNoDir, shsNoDir, sub, silently, verbosely, escaping, print_stdout, print_commands, tracing, errExit
         -- * List functions
         , roll, unroll, liftSh
         -- * Running external commands.
         , FoldCallback
         , run, run_, runFoldLines, cmd
         , (-|-), lastStderr, setStdin, lastExitCode
         , command, command_, command1, command1_
         , sshPairs, sshPairs_
 
         -- * Modifying and querying environment.
         , setenv, get_env, get_env_text, get_env_def, appendToPath

         -- * Environment directory
         , cd, chdir, pwd

         -- * Printing
         , echo, echo_n, echo_err, echo_n_err, inspect, inspect_err
         , tag, trace, show_command

         -- * Querying filesystem.
         , ls, lsT, test_e, test_f, test_d, test_s, which

         -- * Filename helpers
         , absPath, (</>), (<.>), canonic, canonicalize, relPath, relativeTo
         , hasExt

         -- * Manipulating filesystem.
         , mv, rm, rm_f, rm_rf, cp, cp_r, mkdir, mkdir_p, mkdirTree

         -- * reading/writing Files
         , readfile, readBinary, writefile, appendfile, touchfile, withTmpDir

         -- * exiting the program
         , exit, errorExit, quietExit, terror

         -- * Exceptions
         , catchany, catch_sh, finally_sh 
         , ShellyHandler(..), catches_sh
         , catchany_sh

         -- * convert between Text and FilePath
         , toTextIgnore, toTextWarn, fromText

         -- * Utilities.
         , (<$>), (<$$>), whenM, unlessM, time

         -- * Re-exported for your convenience
         , liftIO, when, unless, FilePath

         -- * internal functions for writing extensions
         , get, put

         -- * find functions 
         , find, findWhen, findFold
         , findDirFilter, findDirFilterWhen, findFoldDirFilter
         ) where

import Prelude hiding (FilePath)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception hiding (handle)

import Filesystem.Path(FilePath)

import qualified Shelly as S

import Shelly(
      (</>), (<.>), hasExt
    , (<$$>), whenM, unlessM, toTextIgnore
    , fromText, catchany
    , FoldCallback)

import Data.Maybe(fromMaybe)
import Shelly.Base(State)
import Data.ByteString (ByteString)

import Data.Tree(Tree)

import Data.Text.Lazy as LT hiding (concat, all, find, cons)

default (LT.Text)


-- | This type is a simple wrapper for a type "Shelly.Sh".
-- "Sh" contains a list of results. 
newtype Sh a = Sh { unSh :: S.Sh [a] }

instance Functor Sh where
    fmap f = Sh . fmap (fmap f) . unSh    

instance Monad Sh where
    return  = Sh . return . return 
    a >>= f = Sh $ fmap concat $ mapM (unSh . f) =<< unSh a
    a >> b  = Sh $ unSh a >> unSh b

instance Applicative Sh where
    pure = return
    (<*>) = ap

instance MonadPlus Sh where
    mzero = Sh $ return []
    mplus a b = Sh $ liftA2 (++) (unSh a) (unSh b)

instance MonadIO Sh where
    liftIO = sh1 liftIO

-------------------------------------------------------
-- converters

sh0 :: S.Sh a -> Sh a
sh0 = Sh . fmap return

sh1 :: (a -> S.Sh b) -> (a -> Sh b) 
sh1 f = \a -> sh0 (f a)

sh2 :: (a1 -> a2 -> S.Sh b) -> (a1 -> a2 -> Sh b) 
sh2 f = \a b -> sh0 (f a b)

sh3 :: (a1 -> a2 -> a3 -> S.Sh b) -> (a1 -> a2 -> a3 -> Sh b) 
sh3 f = \a b c -> sh0 (f a b c)

sh4 :: (a1 -> a2 -> a3 -> a4 -> S.Sh b) -> (a1 -> a2 -> a3 -> a4 -> Sh b) 
sh4 f = \a b c d -> sh0 (f a b c d)

sh0s :: S.Sh [a] -> Sh a
sh0s = Sh

sh1s :: (a -> S.Sh [b]) -> (a -> Sh b) 
sh1s f = \a -> sh0s (f a)

{-  Just in case ...
sh2s :: (a1 -> a2 -> S.Sh [b]) -> (a1 -> a2 -> Sh b) 
sh2s f = \a b -> sh0s (f a b)

sh3s :: (a1 -> a2 -> a3 -> S.Sh [b]) -> (a1 -> a2 -> a3 -> Sh b) 
sh3s f = \a b c -> sh0s (f a b c)
-}

lift1 :: (S.Sh a -> S.Sh b) -> (Sh a -> Sh b)
lift1 f = Sh . (mapM (f . return) =<< ) . unSh

lift2 :: (S.Sh a -> S.Sh b -> S.Sh c) -> (Sh a -> Sh b -> Sh c)
lift2 f a b = Sh $ join $ liftA2 (mapM2 f') (unSh a) (unSh b)
    where f' = \x y -> f (return x) (return y)

mapM2 :: Monad m => (a -> b -> m c)-> [a] -> [b] -> m [c]
mapM2 f as bs = sequence $ liftA2 f as bs 

-----------------------------------------------------------

-- | Unpack list of results.
unroll :: Sh a -> Sh [a]
unroll = Sh . fmap return . unSh 

-- | Pack list of results. It performs "concat" inside "Sh".
roll :: Sh [a] -> Sh a
roll = Sh . fmap concat . unSh

-- | Transform result as list. It can be useful for filtering. 
liftSh :: ([a] -> [b]) -> Sh a -> Sh b
liftSh f = Sh . fmap f . unSh

------------------------------------------------------------------
-- Entering Sh

-- | Enter a Sh from (Monad)IO. The environment and working directories are
-- inherited from the current process-wide values. Any subsequent changes in
-- processwide working directory or environment are not reflected in the
-- running Sh.
shelly :: MonadIO m => Sh a -> m [a]
shelly = S.shelly . unSh

-- | Performs "shelly" and then an empty action @return ()@. 
shs :: MonadIO m => Sh () -> m ()
shs a = shelly a >> return ()

-- | Using this entry point does not create a @.shelly@ directory in the case
-- of failure. Instead it logs directly into the standard error stream (@stderr@).
shellyNoDir :: MonadIO m => Sh a -> m [a]
shellyNoDir = S.shellyNoDir . unSh

-- | Performs "shellyNoDir" and then an empty action @return ()@.
shsNoDir :: MonadIO m => Sh () -> m ()
shsNoDir a = shellyNoDir a >> return ()

-- | Enter a sub-Sh that inherits the environment
-- The original state will be restored when the sub-Sh completes.
-- Exceptions are propagated normally.
sub :: Sh a -> Sh a
sub = lift1 S.sub

-- | Create a sub-Sh in which external command outputs are not echoed.
-- Also commands are not printed.
-- See "sub".
silently :: Sh a -> Sh a
silently = lift1 S.silently

-- | Create a sub-Sh in which external command outputs are echoed.
-- Executed commands are printed
-- See "sub".
verbosely :: Sh a -> Sh a
verbosely = lift1 S.verbosely

-- | Create a sub-Sh with shell character escaping on or off
escaping :: Bool -> Sh a -> Sh a
escaping b = lift1 (S.escaping b)

-- | Create a sub-Sh with stdout printing on or off
print_stdout :: Bool -> Sh a -> Sh a
print_stdout b = lift1 (S.print_stdout b)

-- | Create a sub-Sh with command echoing on or off
print_commands :: Bool -> Sh a -> Sh a
print_commands b = lift1 (S.print_commands b)

-- | Create a sub-Sh where commands are not traced
-- Defaults to True.
-- You should only set to False temporarily for very specific reasons
tracing :: Bool -> Sh a -> Sh a
tracing b = lift1 (S.tracing b)

-- | named after bash -e errexit. Defaults to @True@.
-- When @True@, throw an exception on a non-zero exit code.
-- When @False@, ignore a non-zero exit code.
-- Not recommended to set to @False@ unless you are specifically checking the error code with 'lastExitCode'.
errExit :: Bool -> Sh a -> Sh a
errExit b = lift1 (S.errExit b)


------------------------------------------------------------------
-- Running external commands. 

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
run :: FilePath -> [Text] -> Sh Text
run a b = sh0 $ S.run a b

-- | The same as "run", but return () instead of the stdout content.
run_ :: FilePath -> [Text] -> Sh ()
run_ a b = sh0 $ S.run_ a b

-- | used by 'run'. fold over stdout line-by-line as it is read to avoid keeping it in memory
-- stderr is still being placed in memory under the assumption it is always relatively small
runFoldLines :: a -> FoldCallback a -> FilePath -> [Text] -> Sh a
runFoldLines a cb fp ts = sh0 $ S.runFoldLines a cb fp ts

-- | Pipe operator. set the stdout the first command as the stdin of the second.
(-|-) :: Sh Text -> Sh b -> Sh b
(-|-) = lift2 (S.-|-)

-- | The output of last external command. See "run".
lastStderr :: Sh Text
lastStderr = sh0 S.lastStderr

-- | set the stdin to be used and cleared by the next "run".
setStdin :: Text -> Sh ()
setStdin = sh1 S.setStdin 

-- | The exit code from the last command.
-- Unless you set 'errExit' to False you won't get a chance to use this: a non-zero exit code will throw an exception.
lastExitCode :: Sh Int
lastExitCode = sh0 S.lastExitCode

-- | bind some arguments to run for re-use
-- Example: @monit = command "monit" ["-c", "monitrc"]@
command :: FilePath -> [Text] -> [Text] -> Sh Text
command = sh3 S.command

-- | bind some arguments to "run_" for re-use
-- Example: @monit_ = command_ "monit" ["-c", "monitrc"]@
command_ :: FilePath -> [Text] -> [Text] -> Sh ()
command_ = sh3 S.command_


-- | bind some arguments to run for re-use, and expect 1 argument
-- Example: @git = command1 "git" []; git "pull" ["origin", "master"]@
command1 :: FilePath -> [Text] -> Text -> [Text] -> Sh Text
command1 = sh4 S.command1

-- | bind some arguments to run for re-use, and expect 1 argument
-- Example: @git_ = command1_ "git" []; git+ "pull" ["origin", "master"]@
command1_ :: FilePath -> [Text] -> Text -> [Text] -> Sh ()
command1_ = sh4 S.command1_

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
sshPairs :: Text -> [(FilePath, [Text])] -> Sh Text
sshPairs = sh2 S.sshPairs

-- | same as 'sshPairs', but returns ()
sshPairs_ :: Text -> [(FilePath, [Text])] -> Sh ()
sshPairs_ = sh2 S.sshPairs_

-------------------------------------------------------------------
-- Modifying and querying environment. 

-- | Set an environment variable. The environment is maintained in Sh
-- internally, and is passed to any external commands to be executed.
setenv :: Text -> Text -> Sh ()
setenv = sh2 S.setenv

-- | Fetch the current value of an environment variable.
-- if non-existant or empty text, will be Nothing
get_env :: Text -> Sh (Maybe Text)
get_env = sh1 S.get_env

-- | Fetch the current value of an environment variable. Both empty and
-- non-existent variables give empty string as a result.
get_env_text :: Text -> Sh Text
get_env_text = sh1 S.get_env_text

-- | Fetch the current value of an environment variable. Both empty and
-- non-existent variables give the default value as a result
get_env_def :: Text -> Text -> Sh Text
get_env_def a d = sh0 $ fmap (fromMaybe d) $ S.get_env a
{-# DEPRECATED get_env_def "use fromMaybe DEFAULT get_env" #-}

-- | add the filepath onto the PATH env variable
-- FIXME: only effects the PATH once the process is ran, as per comments in 'which'
appendToPath :: FilePath -> Sh ()
appendToPath = sh1 S.appendToPath

-------------------------------------------------------------
-- Environment directory 

-- | Change current working directory of Sh. This does *not* change the
-- working directory of the process we are running it. Instead, Sh keeps
-- track of its own working directory and builds absolute paths internally
-- instead of passing down relative paths. This may have performance
-- repercussions if you are doing hundreds of thousands of filesystem
-- operations. You will want to handle these issues differently in those cases.
cd :: FilePath -> Sh ()
cd = sh1 S.cd

-- | "cd", execute a Sh action in the new directory and then pop back to the original directory
chdir :: FilePath -> Sh a -> Sh a
chdir p = lift1 (S.chdir p)

-- | Obtain the current (Sh) working directory.
pwd :: Sh FilePath
pwd = sh0 S.pwd

-----------------------------------------------------------------
-- Printing 

-- | Echo text to standard (error, when using _err variants) output. The _n
-- variants do not print a final newline.
echo, echo_n_err, echo_err, echo_n :: Text -> Sh ()

echo        = sh1 S.echo
echo_n_err  = sh1 S.echo_n_err
echo_err    = sh1 S.echo_err
echo_n      = sh1 S.echo_n

-- | a print lifted into Sh
inspect :: Show s => s -> Sh ()
inspect = sh1 S.inspect

-- | a print lifted into Sh using stderr
inspect_err :: Show s => s -> Sh ()
inspect_err = sh1 S.inspect_err

-- | same as 'trace', but use it combinator style
tag :: Sh a -> Text -> Sh a
tag a t = lift1 (flip S.tag t) a

-- | internally log what occured.
-- Log will be re-played on failure.
trace :: Text -> Sh ()
trace = sh1 S.trace

show_command :: FilePath -> [Text] -> Text
show_command = S.show_command

------------------------------------------------------------------
-- Querying filesystem

-- | List directory contents. Does *not* include \".\" and \"..\", but it does
-- include (other) hidden files.
ls :: FilePath -> Sh FilePath
ls = sh1s S.ls

-- | Get back [Text] instead of [FilePath]
lsT :: FilePath -> Sh Text
lsT = sh1s S.lsT

-- | Does a path point to an existing filesystem object?
test_e :: FilePath -> Sh Bool
test_e = sh1 S.test_e

-- | Does a path point to an existing file?
test_f :: FilePath -> Sh Bool
test_f = sh1 S.test_f

-- | Does a path point to an existing directory?
test_d :: FilePath -> Sh Bool
test_d = sh1 S.test_d

-- | Does a path point to a symlink?
test_s :: FilePath -> Sh Bool
test_s = sh1 S.test_s

-- | Get a full path to an executable on @PATH@, if exists. FIXME does not
-- respect setenv'd environment and uses @findExecutable@ which uses the @PATH@ inherited from the process
-- environment.
-- FIXME: findExecutable does not maintain a hash of existing commands and does a ton of file stats
which :: FilePath -> Sh (Maybe FilePath)
which = sh1 S.which

---------------------------------------------------------------------
-- Filename helpers

-- | Make a relative path absolute by combining with the working directory.
-- An absolute path is returned as is.
-- To create a relative path, use 'path'.
absPath :: FilePath -> Sh FilePath
absPath = sh1 S.absPath

-- | makes an absolute path.
-- Like 'canonicalize', but on an exception returns 'path'
canonic :: FilePath -> Sh FilePath
canonic = sh1 S.canonic

-- | Obtain a (reasonably) canonic file path to a filesystem object. Based on
-- "canonicalizePath" in system-fileio.
canonicalize :: FilePath -> Sh FilePath
canonicalize = sh1 S.canonicalize

-- | Makes a relative path relative to the current Sh working directory.
-- An absolute path is returned as is.
-- To create an absolute path, use 'absPath'
relPath :: FilePath -> Sh FilePath
relPath = sh1 S.relPath

-- | make the second path relative to the first
-- Uses 'Filesystem.stripPrefix', but will canonicalize the paths if necessary
relativeTo :: FilePath -- ^ anchor path, the prefix
           -> FilePath -- ^ make this relative to anchor path
           -> Sh FilePath
relativeTo = sh2 S.relativeTo

-------------------------------------------------------------
-- Manipulating filesystem

-- | Currently a "renameFile" wrapper. TODO: Support cross-filesystem
-- move. TODO: Support directory paths in the second parameter, like in "cp".
mv :: FilePath -> FilePath -> Sh ()
mv = sh2 S.mv

-- | Remove a file.
-- Does fail if the file does not exist (use 'rm_f' instead) or is not a file.
rm :: FilePath -> Sh ()
rm = sh1 S.rm

-- | Remove a file. Does not fail if the file does not exist.
-- Does fail if the file is not a file.
rm_f :: FilePath -> Sh ()
rm_f = sh1 S.rm_f

-- | A swiss army cannon for removing things. Actually this goes farther than a
-- normal rm -rf, as it will circumvent permission problems for the files we
-- own. Use carefully.
-- Uses 'removeTree'
rm_rf :: FilePath -> Sh ()
rm_rf = sh1 S.rm_rf

-- | Copy a file. The second path could be a directory, in which case the
-- original file name is used, in that directory.
cp :: FilePath -> FilePath -> Sh ()
cp = sh2 S.cp

-- | Copy a file, or a directory recursively.
cp_r :: FilePath -> FilePath -> Sh ()
cp_r = sh2 S.cp_r

-- | Create a new directory (fails if the directory exists).
mkdir :: FilePath -> Sh ()
mkdir = sh1 S.mkdir

-- | Create a new directory, including parents (succeeds if the directory
-- already exists).
mkdir_p :: FilePath -> Sh ()
mkdir_p = sh1 S.mkdir_p

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
mkdirTree = sh1 S.mkdirTree

-- | (Strictly) read file into a Text.
-- All other functions use Lazy Text.
-- So Internally this reads a file as strict text and then converts it to lazy text, which is inefficient
readfile :: FilePath -> Sh Text
readfile = sh1 S.readfile

-- | wraps ByteSting readFile
readBinary :: FilePath -> Sh ByteString
readBinary = sh1 S.readBinary

-- | Write a Lazy Text to a file.
writefile :: FilePath -> Text -> Sh ()
writefile = sh2 S.writefile

-- | Update a file, creating (a blank file) if it does not exist.
touchfile :: FilePath -> Sh ()
touchfile = sh1 S.touchfile

-- | Append a Lazy Text to a file.
appendfile :: FilePath -> Text -> Sh ()
appendfile = sh2 S.appendfile

-- | Create a temporary directory and pass it as a parameter to a Sh
-- computation. The directory is nuked afterwards.
withTmpDir :: (FilePath -> Sh a) -> Sh a
withTmpDir f = Sh $ S.withTmpDir (unSh . f)

-----------------------------------------------------------------
-- find

-- | List directory recursively (like the POSIX utility "find").
-- listing is relative if the path given is relative.
-- If you want to filter out some results or fold over them you can do that with the returned files.
-- A more efficient approach is to use one of the other find functions.
find :: FilePath -> Sh FilePath
find = sh1s S.find

-- | 'find' that filters the found files as it finds.
-- Files must satisfy the given filter to be returned in the result.
findWhen :: (FilePath -> Sh Bool) -> FilePath -> Sh FilePath
findWhen p a = Sh $ S.findWhen (fmap and . unSh . p) a

-- | Fold an arbitrary folding function over files froma a 'find'.
-- Like 'findWhen' but use a more general fold rather than a filter.
findFold :: (a -> FilePath -> Sh a) -> a -> FilePath -> Sh a
findFold cons nil a = Sh $ S.findFold cons' nil' a
    where nil'  = return nil
          cons' as dir = unSh $ roll $ mapM (flip cons dir) as

-- | 'find' that filters out directories as it finds
-- Filtering out directories can make a find much more efficient by avoiding entire trees of files.
findDirFilter :: (FilePath -> Sh Bool) -> FilePath -> Sh FilePath
findDirFilter p a = Sh $ S.findDirFilter (fmap and . unSh . p) a
    
-- | similar 'findWhen', but also filter out directories
-- Alternatively, similar to 'findDirFilter', but also filter out files
-- Filtering out directories makes the find much more efficient
findDirFilterWhen :: (FilePath -> Sh Bool) -- ^ directory filter
                  -> (FilePath -> Sh Bool) -- ^ file filter
                  -> FilePath -- ^ directory
                  -> Sh FilePath
findDirFilterWhen dirPred filePred a = 
    Sh $ S.findDirFilterWhen  
            (fmap and . unSh . dirPred) 
            (fmap and . unSh . filePred)
            a


-- | like 'findDirFilterWhen' but use a folding function rather than a filter
-- The most general finder: you likely want a more specific one
findFoldDirFilter :: (a -> FilePath -> Sh a) -> a -> (FilePath -> Sh Bool) -> FilePath -> Sh a
findFoldDirFilter cons nil p a = Sh $ S.findFoldDirFilter cons' nil' p' a
    where p'    = fmap and . unSh . p
          nil'  = return nil
          cons' as dir = unSh $ roll $ mapM (flip cons dir) as
           
-----------------------------------------------------------
-- exiting the program 

exit :: Int -> Sh ()
exit = sh1 S.exit

errorExit :: Text -> Sh ()
errorExit = sh1 S.errorExit

-- | for exiting with status > 0 without printing debug information
quietExit :: Int -> Sh ()
quietExit = sh1 S.quietExit

-- | fail that takes a Text
terror :: Text -> Sh a
terror = sh1 S.terror

------------------------------------------------------------
-- Utilities

-- | Catch an exception in the Sh monad.
catch_sh :: (Exception e) => Sh a -> (e -> Sh a) -> Sh a
catch_sh a f = Sh $ S.catch_sh (unSh a) (unSh . f)

-- | Catch an exception in the Sh monad.
catchany_sh :: Sh a -> (SomeException -> Sh a) -> Sh a
catchany_sh = catch_sh


-- | Catch an exception in the Sh monad.
finally_sh :: Sh a -> Sh b -> Sh a
finally_sh = lift2 S.finally_sh

-- | Run a Sh computation and collect timing  information.
time :: Sh a -> Sh (Double, a)
time = lift1 S.time

-- | You need this when using 'catches_sh'.
data ShellyHandler a = forall e . Exception e => ShellyHandler (e -> Sh a)

-- | Catch multiple exceptions in the Sh monad.
catches_sh :: Sh a -> [ShellyHandler a] -> Sh a
catches_sh a hs = Sh $ S.catches_sh (unSh a) (fmap convert hs)
    where convert :: ShellyHandler a -> S.ShellyHandler [a]
          convert (ShellyHandler f) = S.ShellyHandler (unSh . f)

------------------------------------------------------------
-- convert between Text and FilePath 

toTextWarn :: FilePath -> Sh Text
toTextWarn = sh1 S.toTextWarn

-------------------------------------------------------------
-- internal functions for writing extension 

get :: Sh State
get = sh0 S.get

put :: State -> Sh ()
put = sh1 S.put

--------------------------------------------------------
-- polyvariadic vodoo

-- | Converter for the variadic argument version of 'run' called 'cmd'.
class ShellArg a where toTextArg :: a -> Text
instance ShellArg Text     where toTextArg = id
instance ShellArg FilePath where toTextArg = toTextIgnore


-- Voodoo to create the variadic function 'cmd'
class ShellCommand t where
    cmdAll :: FilePath -> [Text] -> t

instance ShellCommand (Sh Text) where
    cmdAll fp args = run fp args

instance (s ~ Text, Show s) => ShellCommand (Sh s) where
    cmdAll fp args = run fp args

-- note that Sh () actually doesn't work for its case (_<- cmd) when there is no type signature
instance ShellCommand (Sh ()) where
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

