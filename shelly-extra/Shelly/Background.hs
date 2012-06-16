{-# LANGUAGE OverloadedStrings #-}
module Shelly.Background (
   -- * Running external commands asynchronously.
   jobs, background, getBgResult, BgResult
) where

import Shelly
import Control.Concurrent
import Control.Exception (finally)
import qualified Control.Concurrent.MSem as Sem

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
      result <-
        finally (shelly $ (put state >> proc))
                (Sem.signal manager >> return ()) -- open a spot back up
      putMVar mvar result
    return $ BgResult mvar

