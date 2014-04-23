{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Shelly.Conduit where

import           Conduit
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.Trans.Loop
import           Control.Monad.Trans.Reader ( ask )
import           Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.Conduit.List as CL
import           Data.Maybe
import           Data.Text as T
import qualified Data.Text.IO as TIO
import           Prelude hiding ( FilePath )
import           Shelly hiding ( exit )
import           Shelly.Base
import           System.Exit ( ExitCode(..) )
import           System.IO hiding ( FilePath )
import           System.Process

bufSize :: Int
bufSize = 64 * 1024

runConduit :: FilePath -> [Text] -> [StdHandle]
           -> Conduit S.ByteString (ResourceT Sh) S.ByteString
runConduit exe args reusedHandles = do
    -- clear stdin before beginning command execution
    origstate <- lift2 $ get
    let mStdin = sStdin origstate
    lift2 $ put $ origstate { sStdin = Nothing, sCode = 0, sStderr = T.empty }
    state <- lift2 $ get
    stateRef <- lift2 $ Sh ask

    let cmdString = show_command exe args
    when (sPrintCommands state) $ lift2 $ echo cmdString
    lift2 $ trace cmdString

    bracketP (createp state stateRef) closep $ \(cin,cout,_cerr,ph) -> do
        liftIO $ case mStdin of
          Just input -> TIO.hPutStr cin input
          Nothing -> return ()

        end <- repeatLoopT $ do
          -- if process's outputs are available, then yields them.
          repeatLoopT $ do
            b <- liftIO $ hReady' cout
            when (not b) exit
            out <- liftIO $ S.hGetSome cout bufSize
            void $ lift . lift $ yield out

          -- if process exited, then exit
          end <- liftIO $ getProcessExitCode ph
          when (isJust end) $ exitWith end

          -- if upper stream ended, then exit
          inp <- lift await
          when (isNothing inp) $ exitWith Nothing

          -- put input to process
          liftIO $ S.hPut cin $ fromJust inp
          liftIO $ hFlush cin

        -- uppstream or process is done.
        -- process rest outputs.
        liftIO $ hClose cin
        repeatLoopT $ do
          out <- liftIO $ S.hGetSome cout bufSize
          when (S.null out) exit
          lift $ yield out

        ec <- liftIO $ maybe (waitForProcess' ph) return end
        let code = case ec of
                ExitSuccess -> 0
                ExitFailure n -> n
        lift2 $ modify $ \state' -> state' { sCode = code }

        case (sErrExit state, ec) of
          (True,  ExitFailure n) -> do
              newState <- lift2 get
              liftIO $ E.throwIO $ RunFailed exe args n (sStderr newState)
          _ -> return ()

  where
    lift2 = lift . lift

    createp state stateRef =
        flip runSh stateRef $ (sRun state) reusedHandles state exe args

    closep = \(_,_,_,procH) -> liftIO $ terminateProcess procH

    hReady' h =
      hReady h `E.catch` \(E.SomeException _) -> return False
    waitForProcess' ph =
      waitForProcess ph `E.catch` \(E.SomeException _) -> return ExitSuccess

runSource :: FilePath -> [Text] -> [StdHandle]
          -> Producer (ResourceT Sh) S.ByteString
runSource exe args reusedHandles =
    toProducer $ CL.sourceNull $= runConduit exe args reusedHandles
