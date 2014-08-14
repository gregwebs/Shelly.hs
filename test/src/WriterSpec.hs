{-# LANGUAGE OverloadedStrings #-}
module WriterSpec ( writerSpec ) where

import TestInit
import Prelude hiding (FilePath)

import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import Data.Text (Text)
default (Text)

writerSpec :: Spec
writerSpec =
  describe "withOutputWriter" $
    it "calls writer function with handler and stdout output" $ do
      outputVar <- newEmptyMVar
      shelly
        . withOutputWriter (const $ putMVar outputVar)
        $ run_ "echo" ["single line output"]
      result <- takeMVar outputVar
      assertEqual "expecting output" "single line output" result
