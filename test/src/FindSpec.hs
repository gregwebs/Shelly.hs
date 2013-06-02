{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module FindSpec ( findSpec ) where

import Test.HUnit
import Test.Hspec
import Data.List (sort)
import Shelly

findSpec :: Spec
findSpec = do
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
      res <- shelly $ cd "src" >> ls "."
      sort res @?= sort ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs",
                         "./FindSpec.hs", "./Help.hs", "./MoveSpec.hs",
                         "./ReadFileSpec.hs", "./TestMain.hs",
                         "./WriteSpec.hs", "./drain.hs", "./drain.sh", "./sleep.hs",
                         "./RunSpec.hs", "./printer.sh"]



    it "finds relative files" $ do
      res <- shelly $ cd "src" >> find "."
      sort res @?= sort ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs",
                         "./FindSpec.hs", "./Help.hs", "./MoveSpec.hs",
                         "./ReadFileSpec.hs", "./TestMain.hs",
                         "./WriteSpec.hs", "./drain.hs", "./drain.sh", "./sleep.hs",
                         "./RunSpec.hs", "./printer.sh"]

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
      res <- shelly $ find "src"
      sort res @?= sort ["src/CopySpec.hs", "src/EnvSpec.hs", "src/FailureSpec.hs",
                         "src/FindSpec.hs", "src/Help.hs", "src/MoveSpec.hs", "src/ReadFileSpec.hs",
                         "src/TestMain.hs", "src/WriteSpec.hs", "src/drain.hs",
                         "src/drain.sh", "src/sleep.hs", "src/RunSpec.hs", "src/printer.sh"]

    it "lists absolute files" $ do
      res <- shelly $ relPath "src" >>= find >>= mapM (relativeTo "src")
      sort res @?= sort ["CopySpec.hs", "EnvSpec.hs", "FailureSpec.hs", "FindSpec.hs",
                         "Help.hs", "MoveSpec.hs", "ReadFileSpec.hs", "TestMain.hs",
                         "WriteSpec.hs", "drain.hs", "drain.sh", "sleep.hs",
                         "RunSpec.hs", "printer.sh"]
