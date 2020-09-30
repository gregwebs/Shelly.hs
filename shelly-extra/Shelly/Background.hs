{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ExistentialQuantification #-}
-- | A futures implementation that integrates with shelly
--
-- > jobs 5 (\job -> background job (sleep 2) >> background job (sleep 1))
--
-- 'jobs' will wait for all concurrent jobs to finish.
-- The argument to jobs is the maximum number of concurrent tasks.
-- Generally shell scripts contain a lot of quick commands, but when you have the occasional command that is noticeably long and independent of other commands, you can easily run it concurrently.
module Shelly.Background (
   -- * Running external commands asynchronously.
   jobs, background, killAllJobs
) where

import Shelly
import qualified Control.Concurrent.MSemN as Sem
import Data.IORef
import Control.Concurrent.Async
import Control.Monad.Trans ( MonadIO )
import Control.Monad ( void )

-- | Create a 'BgJobManager' that has a 'limit' on the max number of background tasks.
-- an invocation of jobs is independent of any others, and not tied to the Sh monad in any way.
-- This blocks the execution of the program until all 'background' jobs are finished.
jobs :: Int -> (BgJobManager -> Sh a) -> Sh a
jobs limit action = do
    unless (limit > 0) $ terror "expected limit to be > 0"
    availableJobsSem <- liftIO $ Sem.new limit
    res <- liftIO (newIORef []) >>= action . BgJobManager availableJobsSem
    liftIO $ Sem.wait availableJobsSem limit
    return res

-- | The manager tracks the number of jobs. Register your 'background' jobs with it.
data BgJobManager = BgJobManager (Sem.MSemN Int) (IORef [Async ()])

killAllJobs :: MonadIO m => BgJobManager -> m ()
killAllJobs man = getJobs man >>= mapM_ (liftIO . cancel)
  where
    getJobs :: MonadIO m => BgJobManager -> m [Async ()]
    getJobs (BgJobManager _ asyncs) = liftIO $ readIORef asyncs

-- | Run the 'Sh' task asynchronously in the background,
-- immediately returns the 'Async' promise.
-- The background task will inherit the current Sh context
-- The 'BgJobManager' ensures the max jobs limit must be sufficient for the parent and all children.
background :: BgJobManager -> Sh a -> Sh (Async a)
background (BgJobManager manager asyncs) proc = do
    -- take up a spot
    -- It is important to do this before forkIO:
    -- It ensures that that jobs will block and the program won't exit before our jobs are done
    -- On the other hand, a user might not expect 'jobs' to block
    liftIO $ Sem.wait manager 1
    a <- asyncSh $ finally_sh proc (liftIO $ Sem.signal manager 1)
    liftIO $ do
        link a
        -- to make our types easier,
        -- we want [Async ()] for killall
        -- since killall doesn't care about return types
        -- perhaps there is a more elegant way to accomplish this?
        b <- async $ void $ wait a
        link2 a b
        atomicModifyIORef' asyncs (\as -> (b:as, ()))
        return a
