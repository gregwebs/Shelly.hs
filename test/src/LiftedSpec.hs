{-# LANGUAGE OverloadedStrings #-}

module LiftedSpec ( liftedSpec ) where

import Test.HUnit hiding (path)
import Test.Hspec
import Shelly.Lifted
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Maybe
import Test.Hspec.HUnit ()

liftedSpec :: Spec
liftedSpec =
  describe "basic actions" $
    it "lifted sub" $ do
      xs <- shelly $
          runMaybeT $ do
              echo "Hello!"
              sub $ withTmpDir $ \p -> wait =<< (async $ do
                  writefile (p </> "test.txt") "hello"
                  readfile (p </> "test.txt")
                  )
      xs @?= Just "hello"
