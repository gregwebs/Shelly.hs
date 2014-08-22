{-# LANGUAGE OverloadedStrings #-}
module LogWithSpec ( logWithSpec ) where

import TestInit
import Prelude hiding (FilePath)

import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import Data.Text (Text)
default (Text)

logWithSpec :: Spec
logWithSpec =
  describe "withOutputWriter" $
    it "calls writer function with handler and stdout output" $ do
      outputVar <- newEmptyMVar
      shelly $ log_stdout_with (putMVar outputVar)
        $ run_ "echo" ["single line output"]
      result <- takeMVar outputVar
      assertEqual "expecting output" "single line output" result
