{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module FindSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit
import Test.Hspec.Monadic

import Shelly
import Shelly.Find

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "relativeTo" $ do
    it "relative to non-existent dir" $ do
      res <- shelly $ relativeTo "rel/" "rel/foo"
      res @?= "foo"
      res <- shelly $ relativeTo "rel" "rel/foo"
      res @?= "foo"

    it "relative to existing dir" $ do
      res <- shelly $ relativeTo "test/" "test/drain.hs"
      res @?= "drain.hs"
      res <- shelly $ relativeTo "test" "test/drain.hs"
      res @?= "drain.hs"

    it "abs path relative to existing dir" $ do
      res <- shelly $ relativeTo "test/" "/Users/gweber/proj/hs/Shelly.hs/test/drain.hs"
      res @?= "drain.hs"
      res <- shelly $ relativeTo "test" "/Users/gweber/proj/hs/Shelly.hs/test/drain.hs"
      res @?= "drain.hs"

  describe "find" $ do
    it "empty list for empty dir" $ do
      let d = "deleteme"
      res <- shelly $ do
        mkdir_p d
        res <- find d
        rm_rf d
        return res
      res @?= []

    it "lists relative files" $ do
      res <- shelly $ find "test"
      res @?= ["test/CopySpec.hs", "test/drain.hs", "test/drain.sh", "test/FindSpec.hs", "test/main.hs", "test/WriteSpec.hs"]

    it "lists absolute files" $ do
      res <- shelly $ relPath "test" >>= find >>= mapM (relativeTo "test")
      res @?= ["CopySpec.hs", "drain.hs", "drain.sh", "FindSpec.hs", "main.hs", "WriteSpec.hs"]
