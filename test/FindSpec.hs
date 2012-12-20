{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module FindSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit
import Test.Hspec
import Data.List (sort)

import Shelly

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "relativeTo" $ do
    it "relative to non-existent dir" $ do
      res <- shelly $ relativeTo "rel/" "rel/foo"
      res @?= "foo"
      res2 <- shelly $ relativeTo "rel" "rel/foo"
      res2 @?= "foo"

    it "relative to existing dir" $ do
      res <- shelly $ relativeTo "test/" "test/drain.hs"
      res @?= "drain.hs"
      res2 <- shelly $ relativeTo "test" "test/drain.hs"
      res2 @?= "drain.hs"

    it "abs path relative to existing dir" $ do
      res  <- shelly $ do
        d <- pwd
        relativeTo "test/" $ d </> "test/drain.hs"
      res @?= "drain.hs"

  describe "relative listing" $ do
    it "lists relative files" $ do
      res <- shelly $ cd "test" >> ls "."
      sort res @?= ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs", "./FindSpec.hs", "./Help.hs", "./MoveSpec.hs", "./ReadFileSpec.hs", "./WriteSpec.hs", "./data", "./drain.hs", "./drain.sh", "./main.hs"]

    it "finds relative files" $ do
      res <- shelly $ cd "test" >> find "."
      sort res @?= ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs", "./FindSpec.hs", "./Help.hs", "./MoveSpec.hs", "./ReadFileSpec.hs", "./WriteSpec.hs", "./data", "./drain.hs", "./drain.sh", "./main.hs", "./data/zshrc"]

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
      sort res @?= ["test/CopySpec.hs", "test/EnvSpec.hs", "test/FailureSpec.hs", "test/FindSpec.hs", "test/Help.hs", "test/MoveSpec.hs", "test/ReadFileSpec.hs", "test/WriteSpec.hs", "test/data", "test/drain.hs", "test/drain.sh", "test/main.hs", "test/data/zshrc"]

    it "lists absolute files" $ do
      res <- shelly $ relPath "test" >>= find >>= mapM (relativeTo "test")
      sort res @?= ["CopySpec.hs", "EnvSpec.hs", "FailureSpec.hs", "FindSpec.hs", "Help.hs", "MoveSpec.hs", "ReadFileSpec.hs", "WriteSpec.hs", "data", "drain.hs", "drain.sh", "main.hs", "data/zshrc"]
