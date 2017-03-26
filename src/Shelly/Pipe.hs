{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, 
             TypeFamilies, ExistentialQuantification #-}
-- | This module is a wrapper for the module "Shelly". 
-- The only difference is a main type 'Sh'. In this module 
-- 'Sh' contains a list of results. Actual definition of the type 'Sh' is:
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
-- Documentation in this module mostly just reference documentation from
-- the main "Shelly" module.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE ExtendedDefaultRules #-}
-- > {-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- > import Shelly
-- > import Data.Text as T
-- > default (T.Text)
module Shelly.Pipe
       (
         -- * Entering Sh.
         Sh, shs, shelly, shellyFailDir, shsFailDir, sub, silently, verbosely, escaping, print_stdout, print_commands, tracing, errExit, log_stdout_with, log_stderr_with
         -- * List functions
         , roll, unroll, liftSh
         -- * Running external commands.
         , FoldCallback
         , run, run_, runFoldLines, cmd
         , (-|-), lastStderr, setStdin, lastExitCode
         , command, command_, command1, command1_
         , sshPairs, sshPairs_

         -- * Modifying and querying environment.
         , setenv, get_env, get_env_text, get_env_def, appendToPath, prependToPath

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
         , (<$>), whenM, unlessM, time

         -- * Re-exported for your convenience
         , liftIO, when, unless, FilePath

         -- * internal functions for writing extensions
         , get, put

         -- * find functions 
         , find, findWhen, findFold
         , findDirFilter, findDirFilterWhen, findFoldDirFilter
         , followSymlink
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
    , whenM, unlessM, toTextIgnore
    , fromText, catchany
    , FoldCallback)

import Data.Maybe(fromMaybe)
import Shelly.Base(State)
import Data.ByteString (ByteString)

import Data.Tree(Tree)

import Data.Text as T hiding (concat, all, find, cons)


-- | This type is a simple wrapper for a type @Shelly.Sh@.
-- 'Sh' contains a list of results. 
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

instance Alternative Sh where
    empty = mzero
    (<|>) = mplus

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

-- | Pack list of results. It performs @concat@ inside 'Sh'.
roll :: Sh [a] -> Sh a
roll = Sh . fmap concat . unSh

-- | Transform result as list. It can be useful for filtering. 
liftSh :: ([a] -> [b]) -> Sh a -> Sh b
liftSh f = Sh . fmap f . unSh

------------------------------------------------------------------
-- Entering Sh

-- | see 'S.shelly'
shelly :: MonadIO m => Sh a -> m [a]
shelly = S.shelly . unSh

-- | Performs 'shelly' and then an empty action @return ()@. 
shs :: MonadIO m => Sh () -> m ()
shs x = shelly x >> return ()

-- | see 'S.shellyFailDir'
shellyFailDir :: MonadIO m => Sh a -> m [a]
shellyFailDir = S.shellyFailDir . unSh

-- | Performs 'shellyFailDir' and then an empty action @return ()@.
shsFailDir :: MonadIO m => Sh () -> m ()
shsFailDir x = shellyFailDir x >> return ()

-- | see 'S.sub'
sub :: Sh a -> Sh a
sub = lift1 S.sub

-- See 'S.siliently'
silently :: Sh a -> Sh a
silently = lift1 S.silently

-- See 'S.verbosely
verbosely :: Sh a -> Sh a
verbosely = lift1 S.verbosely

-- | see 'S.escaping'
escaping :: Bool -> Sh a -> Sh a
escaping b = lift1 (S.escaping b)

-- | see 'S.log_stdout_with'
log_stdout_with :: (Text -> IO ()) -> Sh a -> Sh a
log_stdout_with logger = lift1 (S.log_stdout_with logger)

-- | see 'S.log_stderr_with'
log_stderr_with :: (Text -> IO ()) -> Sh a -> Sh a
log_stderr_with logger = lift1 (S.log_stdout_with logger)

-- | see 'S.print_stdout'
print_stdout :: Bool -> Sh a -> Sh a
print_stdout b = lift1 (S.print_stdout b)

-- | see 'S.print_commands
print_commands :: Bool -> Sh a -> Sh a
print_commands b = lift1 (S.print_commands b)

-- | see 'S.tracing'
tracing :: Bool -> Sh a -> Sh a
tracing b = lift1 (S.tracing b)

-- | see 'S.errExit'
errExit :: Bool -> Sh a -> Sh a
errExit b = lift1 (S.errExit b)

-- | see 'S.followSymlink'
followSymlink :: Bool -> Sh a -> Sh a
followSymlink b = lift1 (S.followSymlink b)

-- | see 'S.run'
run :: FilePath -> [Text] -> Sh Text
run a b = sh0 $ S.run a b

-- | see 'S.run_'
run_ :: FilePath -> [Text] -> Sh ()
run_ a b = sh0 $ S.run_ a b

-- | see 'S.runFoldLines'
runFoldLines :: a -> FoldCallback a -> FilePath -> [Text] -> Sh a
runFoldLines a cb fp ts = sh0 $ S.runFoldLines a cb fp ts

-- | see 'S.-|-'
(-|-) :: Sh Text -> Sh b -> Sh b
(-|-) = lift2 (S.-|-)

-- | see 'S.lastStderr'
lastStderr :: Sh Text
lastStderr = sh0 S.lastStderr

-- | see 'S.setStdin'
setStdin :: Text -> Sh ()
setStdin = sh1 S.setStdin 

-- | see 'S.lastExitCode'
lastExitCode :: Sh Int
lastExitCode = sh0 S.lastExitCode

-- | see 'S.command'
command :: FilePath -> [Text] -> [Text] -> Sh Text
command = sh3 S.command

-- | see 'S.command_'
command_ :: FilePath -> [Text] -> [Text] -> Sh ()
command_ = sh3 S.command_


-- | see 'S.command1'
command1 :: FilePath -> [Text] -> Text -> [Text] -> Sh Text
command1 = sh4 S.command1

-- | see 'S.command1_'
command1_ :: FilePath -> [Text] -> Text -> [Text] -> Sh ()
command1_ = sh4 S.command1_

-- | see 'S.sshPairs'
sshPairs :: Text -> [(FilePath, [Text])] -> Sh Text
sshPairs = sh2 S.sshPairs

-- | see 'S.sshPairs_'
sshPairs_ :: Text -> [(FilePath, [Text])] -> Sh ()
sshPairs_ = sh2 S.sshPairs_

-- | see 'S.setenv'
setenv :: Text -> Text -> Sh ()
setenv = sh2 S.setenv

-- | see 'S.get_env'
get_env :: Text -> Sh (Maybe Text)
get_env = sh1 S.get_env

-- | see 'S.get_env_text'
get_env_text :: Text -> Sh Text
get_env_text = sh1 S.get_env_text

-- | see 'S.get_env_def'
get_env_def :: Text -> Text -> Sh Text
get_env_def a d = sh0 $ fmap (fromMaybe d) $ S.get_env a
{-# DEPRECATED get_env_def "use fromMaybe DEFAULT get_env" #-}

-- | see 'S.appendToPath'
appendToPath :: FilePath -> Sh ()
appendToPath = sh1 S.appendToPath

-- | see 'S.prependToPath'
prependToPath :: FilePath -> Sh ()
prependToPath = sh1 S.prependToPath

-- | see 'S.cd'
cd :: FilePath -> Sh ()
cd = sh1 S.cd

-- | see 'S.chdir'
chdir :: FilePath -> Sh a -> Sh a
chdir p = lift1 (S.chdir p)

-- | see 'S.pwd'
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

-- | see 'S.inspect'
inspect :: Show s => s -> Sh ()
inspect = sh1 S.inspect

-- | see 'S.inspect_err'
inspect_err :: Show s => s -> Sh ()
inspect_err = sh1 S.inspect_err

-- | see 'S.tag'
tag :: Sh a -> Text -> Sh a
tag a t = lift1 (flip S.tag t) a

-- | see 'S.trace'
trace :: Text -> Sh ()
trace = sh1 S.trace

-- | see 'S.show_command'
show_command :: FilePath -> [Text] -> Text
show_command = S.show_command

------------------------------------------------------------------
-- Querying filesystem

-- | see 'S.ls'
ls :: FilePath -> Sh FilePath
ls = sh1s S.ls

-- | see 'S.lsT'
lsT :: FilePath -> Sh Text
lsT = sh1s S.lsT

-- | see 'S.test_e'
test_e :: FilePath -> Sh Bool
test_e = sh1 S.test_e

-- | see 'S.test_f'
test_f :: FilePath -> Sh Bool
test_f = sh1 S.test_f

-- | see 'S.test_d'
test_d :: FilePath -> Sh Bool
test_d = sh1 S.test_d

-- | see 'S.test_s'
test_s :: FilePath -> Sh Bool
test_s = sh1 S.test_s

-- | see 'S.which
which :: FilePath -> Sh (Maybe FilePath)
which = sh1 S.which

---------------------------------------------------------------------
-- Filename helpers

-- | see 'S.absPath'
absPath :: FilePath -> Sh FilePath
absPath = sh1 S.absPath

-- | see 'S.canonic'
canonic :: FilePath -> Sh FilePath
canonic = sh1 S.canonic

-- | see 'S.canonicalize'
canonicalize :: FilePath -> Sh FilePath
canonicalize = sh1 S.canonicalize

-- | see 'S.relPath'
relPath :: FilePath -> Sh FilePath
relPath = sh1 S.relPath

-- | see 'S.relativeTo'
relativeTo :: FilePath -- ^ anchor path, the prefix
           -> FilePath -- ^ make this relative to anchor path
           -> Sh FilePath
relativeTo = sh2 S.relativeTo

-------------------------------------------------------------
-- Manipulating filesystem

-- | see 'S.mv'
mv :: FilePath -> FilePath -> Sh ()
mv = sh2 S.mv

-- | see 'S.rm'
rm :: FilePath -> Sh ()
rm = sh1 S.rm

-- | see 'S.rm_f'
rm_f :: FilePath -> Sh ()
rm_f = sh1 S.rm_f

-- | see 'S.rm_rf'
rm_rf :: FilePath -> Sh ()
rm_rf = sh1 S.rm_rf

-- | see 'S.cp'
cp :: FilePath -> FilePath -> Sh ()
cp = sh2 S.cp

-- | see 'S.cp_r'
cp_r :: FilePath -> FilePath -> Sh ()
cp_r = sh2 S.cp_r

-- | see 'S.mkdir'
mkdir :: FilePath -> Sh ()
mkdir = sh1 S.mkdir

-- | see 'S.mkdir_p'
mkdir_p :: FilePath -> Sh ()
mkdir_p = sh1 S.mkdir_p

-- | see 'S.mkdirTree'
mkdirTree :: Tree FilePath -> Sh ()
mkdirTree = sh1 S.mkdirTree

-- | see 'S.readFile'
readfile :: FilePath -> Sh Text
readfile = sh1 S.readfile

-- | see 'S.readBinary'
readBinary :: FilePath -> Sh ByteString
readBinary = sh1 S.readBinary

-- | see 'S.writeFile'
writefile :: FilePath -> Text -> Sh ()
writefile = sh2 S.writefile

-- | see 'S.touchFile'
touchfile :: FilePath -> Sh ()
touchfile = sh1 S.touchfile

-- | see 'S.appendFile'
appendfile :: FilePath -> Text -> Sh ()
appendfile = sh2 S.appendfile

-- | see 'S.withTmpDir'
withTmpDir :: (FilePath -> Sh a) -> Sh a
withTmpDir f = Sh $ S.withTmpDir (unSh . f)

-----------------------------------------------------------------
-- find

-- | see 'S.find'
find :: FilePath -> Sh FilePath
find = sh1s S.find

-- | see 'S.findWhen'
findWhen :: (FilePath -> Sh Bool) -> FilePath -> Sh FilePath
findWhen p a = Sh $ S.findWhen (fmap and . unSh . p) a

-- | see 'S.findFold'
findFold :: (a -> FilePath -> Sh a) -> a -> FilePath -> Sh a
findFold cons nil a = Sh $ S.findFold cons' nil' a
    where nil'  = return nil
          cons' as dir = unSh $ roll $ mapM (flip cons dir) as

-- | see 'S.findDirFilter'
findDirFilter :: (FilePath -> Sh Bool) -> FilePath -> Sh FilePath
findDirFilter p a = Sh $ S.findDirFilter (fmap and . unSh . p) a
    
-- | see 'S.findDirFilterWhen'
findDirFilterWhen :: (FilePath -> Sh Bool) -- ^ directory filter
                  -> (FilePath -> Sh Bool) -- ^ file filter
                  -> FilePath -- ^ directory
                  -> Sh FilePath
findDirFilterWhen dirPred filePred a = 
    Sh $ S.findDirFilterWhen  
            (fmap and . unSh . dirPred) 
            (fmap and . unSh . filePred)
            a


-- | see 'S.findFoldDirFilterWhen'
findFoldDirFilter :: (a -> FilePath -> Sh a) -> a -> (FilePath -> Sh Bool) -> FilePath -> Sh a
findFoldDirFilter cons nil p a = Sh $ S.findFoldDirFilter cons' nil' p' a
    where p'    = fmap and . unSh . p
          nil'  = return nil
          cons' as dir = unSh $ roll $ mapM (flip cons dir) as
           
-----------------------------------------------------------
-- exiting the program 

-- | see 'S.exit'
exit :: Int -> Sh ()
exit = sh1 S.exit

-- | see 'S.errorExit'
errorExit :: Text -> Sh ()
errorExit = sh1 S.errorExit

-- | see 'S.quietExit'
quietExit :: Int -> Sh ()
quietExit = sh1 S.quietExit

-- | see 'S.terror'
terror :: Text -> Sh a
terror = sh1 S.terror

------------------------------------------------------------
-- Utilities

-- | see 'S.catch_sh'
catch_sh :: (Exception e) => Sh a -> (e -> Sh a) -> Sh a
catch_sh a f = Sh $ S.catch_sh (unSh a) (unSh . f)

-- | see 'S.catchany_sh'
catchany_sh :: Sh a -> (SomeException -> Sh a) -> Sh a
catchany_sh = catch_sh


-- | see 'S.finally_sh'
finally_sh :: Sh a -> Sh b -> Sh a
finally_sh = lift2 S.finally_sh

-- | see 'S.time'
time :: Sh a -> Sh (Double, a)
time = lift1 S.time

-- | see 'S.ShellyHandler'
data ShellyHandler a = forall e . Exception e => ShellyHandler (e -> Sh a)

-- | see 'S.catches_sh'
catches_sh :: Sh a -> [ShellyHandler a] -> Sh a
catches_sh a hs = Sh $ S.catches_sh (unSh a) (fmap convert hs)
    where convert :: ShellyHandler a -> S.ShellyHandler [a]
          convert (ShellyHandler f) = S.ShellyHandler (unSh . f)

------------------------------------------------------------
-- convert between Text and FilePath 

-- | see 'S.toTextWarn'
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

-- | see 'S.cmd'
cmd :: (ShellCommand result) => FilePath -> result
cmd fp = cmdAll fp []
