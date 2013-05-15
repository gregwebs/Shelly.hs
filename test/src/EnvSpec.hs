{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module EnvSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit hiding (path)
import Test.Hspec
import Prelude hiding (catch, FilePath)
import Shelly
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
 

