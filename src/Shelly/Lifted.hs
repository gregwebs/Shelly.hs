{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings,
             FlexibleInstances, FlexibleContexts, IncoherentInstances,
             TypeFamilies, ExistentialQuantification, RankNTypes #-}

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
module Shelly.Lifted
       (
         module Shelly

         , MonadSh(..)

         -- * Entering Sh.
         , sub
         , silently, verbosely, escaping, print_stdout, print_stderr, print_commands
         , tracing, errExit

         -- * Running external commands.
         , (-|-)

         -- * Environment directory
         , chdir

         -- * Printing
         , tag, time

         -- * reading/writing Files
         , withTmpDir
         ) where

import Shelly hiding
    (
           sub
         , silently, verbosely, escaping, print_stdout, print_stderr, print_commands
         , tracing, errExit

         -- * Running external commands.
         , (-|-)

         -- * Environment directory
         , chdir

         -- * Printing
         , tag, time

         -- * reading/writing Files
         , withTmpDir
    )
import qualified Shelly as S
import Shelly.Base
import Control.Monad ( liftM )
import Prelude hiding ( FilePath )
import Data.Monoid

import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS as RWS
import qualified Control.Monad.Trans.RWS.Strict as Strict

class Monad m => MonadSh m where
    liftSh :: Sh a -> m a

instance MonadSh Sh where
    liftSh = id

instance MonadSh m => MonadSh (IdentityT m) where
    liftSh = IdentityT . liftSh
instance MonadSh m => MonadSh (ListT m) where
    liftSh m = ListT $ do
        a <- liftSh m
        return [a]
instance MonadSh m => MonadSh (MaybeT m) where
    liftSh = MaybeT . liftM Just . liftSh
instance MonadSh m => MonadSh (ContT r m) where
    liftSh m = ContT (liftSh m >>=)
instance (Error e, MonadSh m) => MonadSh (ErrorT e m) where
    liftSh m = ErrorT $ do
        a <- liftSh m
        return (Right a)
instance MonadSh m => MonadSh (ReaderT r m) where
    liftSh = ReaderT . const . liftSh
instance MonadSh m => MonadSh (StateT s m) where
    liftSh m = StateT $ \s -> do
        a <- liftSh m
        return (a, s)
instance MonadSh m => MonadSh (Strict.StateT s m) where
    liftSh m = Strict.StateT $ \s -> do
        a <- liftSh m
        return (a, s)
instance (Monoid w, MonadSh m) => MonadSh (WriterT w m) where
    liftSh m = WriterT $ do
        a <- liftSh m
        return (a, mempty)
instance (Monoid w, MonadSh m) => MonadSh (Strict.WriterT w m) where
    liftSh m = Strict.WriterT $ do
        a <- liftSh m
        return (a, mempty)
instance (Monoid w, MonadSh m) => MonadSh (RWS.RWST r w s m) where
    liftSh m = RWS.RWST $ \_ s -> do
        a <- liftSh m
        return (a, s, mempty)
instance (Monoid w, MonadSh m) => MonadSh (Strict.RWST r w s m) where
    liftSh m = Strict.RWST $ \_ s -> do
        a <- liftSh m
        return (a, s, mempty)

instance MonadSh m => S.ShellCmd (m Text) where
    cmdAll = (liftSh .) . S.run

instance (MonadSh m, s ~ Text, Show s) => S.ShellCmd (m s) where
    cmdAll = (liftSh .) . S.run

instance MonadSh m => S.ShellCmd (m ()) where
    cmdAll = (liftSh .) . S.run_

class Monad m => MonadShControl m where
    data ShM m a :: *
    liftShWith :: ((forall x. m x -> Sh (ShM m x)) -> Sh a) -> m a
    restoreSh :: ShM m a -> m a

instance MonadShControl Sh where
     newtype ShM Sh a = ShSh a
     liftShWith f = f $ liftM ShSh
     restoreSh (ShSh x) = return x
     {-# INLINE liftShWith #-}
     {-# INLINE restoreSh #-}

instance MonadShControl m => MonadShControl (ListT m) where
    newtype ShM (ListT m) a = ListTShM (ShM m [a])
    liftShWith f =
        ListT $ liftM (:[]) $ liftShWith $ \runInSh -> f $ \k ->
            liftM ListTShM $ runInSh $ runListT k
    restoreSh (ListTShM m) = ListT . restoreSh $ m
    {-# INLINE liftShWith #-}
    {-# INLINE restoreSh #-}

instance MonadShControl m => MonadShControl (MaybeT m) where
    newtype ShM (MaybeT m) a = MaybeTShM (ShM m (Maybe a))
    liftShWith f =
        MaybeT $ liftM Just $ liftShWith $ \runInSh -> f $ \k ->
            liftM MaybeTShM $ runInSh $ runMaybeT k
    restoreSh (MaybeTShM m) = MaybeT . restoreSh $ m
    {-# INLINE liftShWith #-}
    {-# INLINE restoreSh #-}

-- instance MonadShControl m
--          => MonadShControl (IdentityT m) where
--     newtype ShM (IdentityT m) a = IdentityTShM (ShM m a)
--     liftShWith f =
--         IdentityT $ defaultLiftShWith f runIdentityT IdentityTShM id
--     restoreSh (IdentityTShM m) = IdentityT . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance (MonadShControl m, Monoid w)
--          => MonadShControl (WriterT w m) where
--     newtype ShM (WriterT w m) a = WriterTShM (ShM m   (a, w))
--     liftShWith f = WriterT $
--         defaultLiftShWith f runWriterT WriterTShM (\x -> (x, mempty))
--     restoreSh (WriterTShM m) = WriterT . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance (MonadShControl m, Monoid w)
--          => MonadShControl (Strict.WriterT w m) where
--     newtype ShM (Strict.WriterT w m) a = StWriterTShM (ShM m (a, w))
--     liftShWith f = Strict.WriterT $
--         defaultLiftShWith f Strict.runWriterT StWriterTShM (\x -> (x, mempty))
--     restoreSh (StWriterTShM m) = Strict.WriterT . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance (MonadShControl m, Error e)
--          => MonadShControl (ErrorT e m) where
--     newtype ShM (ErrorT e m) a = ErrorTShM (ShM m (Either e a))
--     liftShWith f = ErrorT $ defaultLiftShWith f runErrorT ErrorTShM return
--     restoreSh (ErrorTShM m) = ErrorT . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance MonadShControl m => MonadShControl (StateT s m) where
--     newtype ShM (StateT s m) a = StateTShM (ShM m (a, s))
--     liftShWith f = StateT $ \s ->
--         defaultLiftShWith f (`runStateT` s) StateTShM (\x -> (x,s))
--     restoreSh (StateTShM m) = StateT . const . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance MonadShControl m => MonadShControl (Strict.StateT s m) where
--     newtype ShM (Strict.StateT s m) a = StStateTShM (ShM m (a, s))
--     liftShWith f = Strict.StateT $ \s ->
--         defaultLiftShWith f (`Strict.runStateT` s) StStateTShM (\x -> (x,s))
--     restoreSh (StStateTShM m) = Strict.StateT . const . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance MonadShControl m => MonadShControl (ReaderT r m) where
--     newtype ShM (ReaderT r m) a = ReaderTShM (ShM m a)
--     liftShWith f = ReaderT $ \r ->
--         defaultLiftShWith f (`runReaderT` r) ReaderTShM id
--     restoreSh (ReaderTShM m) = ReaderT . const . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance (MonadShControl m, Monoid w)
--          => MonadShControl (RWS.RWST r w s m) where
--     newtype ShM (RWS.RWST r w s m) a = RWSTShM (ShM m (a, s ,w))
--     liftShWith f = RWS.RWST $ \r s ->
--         defaultLiftShWith f (flip (`RWS.runRWST` r) s) RWSTShM
--             (\x -> (x,s,mempty))
--     restoreSh (RWSTShM m) = RWS.RWST . const . const . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

-- instance (MonadShControl m, Monoid w)
--          => MonadShControl (Strict.RWST r w s m) where
--     newtype ShM (Strict.RWST r w s m) a = StRWSTShM (ShM m (a, s, w))
--     liftShWith f = Strict.RWST $ \r s ->
--         defaultLiftShWith f (flip (`Strict.runRWST` r) s) StRWSTShM
--             (\x -> (x,s,mempty))
--     restoreSh (StRWSTShM m) = Strict.RWST . const . const . restoreSh $ m
--     {-# INLINE liftShWith #-}
--     {-# INLINE restoreSh #-}

controlSh :: MonadShControl m => ((forall x. m x -> Sh (ShM m x)) -> Sh (ShM m a)) -> m a
controlSh = liftShWith >=> restoreSh
{-# INLINE controlSh #-}

tag :: (MonadShControl m, MonadSh m) => m a -> Text -> m a
tag action msg = controlSh $ \runInSh -> S.tag (runInSh action) msg

chdir :: MonadShControl m => FilePath -> m a -> m a
chdir dir action = controlSh $ \runInSh -> S.chdir dir (runInSh action)

silently :: MonadShControl m => m a -> m a
silently a = controlSh $ \runInSh -> S.silently (runInSh a)

verbosely :: MonadShControl m => m a -> m a
verbosely a = controlSh $ \runInSh -> S.verbosely (runInSh a)

print_stdout :: MonadShControl m => Bool -> m a -> m a
print_stdout shouldPrint a = controlSh $ \runInSh -> S.print_stdout shouldPrint (runInSh a)

print_stderr :: MonadShControl m => Bool -> m a -> m a
print_stderr shouldPrint a = controlSh $ \runInSh -> S.print_stderr shouldPrint (runInSh a)

print_commands :: MonadShControl m => Bool -> m a -> m a
print_commands shouldPrint a = controlSh $ \runInSh -> S.print_commands shouldPrint (runInSh a)

sub :: MonadShControl m => m a -> m a
sub a = controlSh $ \runInSh -> S.sub (runInSh a)

tracing :: MonadShControl m => Bool -> m a -> m a
tracing shouldTrace action = controlSh $ \runInSh -> S.tracing shouldTrace (runInSh action)

escaping :: MonadShControl m => Bool -> m a -> m a
escaping shouldEscape action = controlSh $ \runInSh -> S.escaping shouldEscape (runInSh action)

errExit :: MonadShControl m => Bool -> m a -> m a
errExit shouldExit action = controlSh $ \runInSh -> S.errExit shouldExit (runInSh action)

(-|-) :: (MonadShControl m, MonadSh m) => m Text -> m b -> m b
one -|- two = controlSh $ \runInSh -> do
    x <- runInSh one
    runInSh $ restoreSh x >>= \x' ->
        controlSh $ \runInSh' -> return x' S.-|- runInSh' two

withTmpDir :: MonadShControl m => (FilePath -> m a) -> m a
withTmpDir action = controlSh $ \runInSh -> S.withTmpDir (fmap runInSh action)

time :: MonadShControl m => m a -> m (Double, a)
time what = controlSh $ \runInSh -> do
    (d, a) <- S.time (runInSh what)
    runInSh $ restoreSh a >>= \x -> return (d, x)
