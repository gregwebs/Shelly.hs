{-# LANGUAGE OverloadedStrings #-}

module LiftedSpec ( liftedSpec ) where

import Test.HUnit hiding (path)
import Test.Hspec
import Shelly.Lifted
import Control.Exception.Lifted
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Maybe
import Test.Hspec.HUnit ()

liftedSpec :: Spec
liftedSpec = do
  describe "basic actions" $ do
    it "lifted sub" $ do
      xs <- shelly $
          runMaybeT $ do
              xs <- sub $ withTmpDir $ \path -> async $ liftSh $ do
                  writefile (path </> "test.txt") "hello"
                  readfile (path </> "test.txt")
              liftSh (echo "Hello!")
              wait xs
      xs @?= Just "hello"
