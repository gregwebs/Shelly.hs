{-# LANGUAGE CPP #-}
module EnvSpec ( envSpec ) where

import TestInit
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding ( FilePath, catch)
#else
import Prelude hiding ( FilePath)
#endif
import Data.Maybe

envSpec :: Spec
envSpec = do
  describe "getting unset env variables" $ do
    it "get_env" $ do
      res <- shelly $ get_env "FOOBARSHELLY"
      assert $ isNothing res

    it "get_env_text" $ do
      res <- shelly $ get_env_text "FOOBARSHELLY"
      assert $ res == ""

  describe "with SHELLY var set" $ do
    it "get_env" $ do
      res <- shelly $ do
        setenv "SHELLY" "test"
        get_env "SHELLY"
      assert $ res == Just "test"

    it "get_env_text" $ do
      res <- shelly $ do
        setenv "SHELLY" "test"
        get_env_text "SHELLY"
      assert $ res == "test"
 

