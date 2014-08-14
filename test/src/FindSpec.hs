module FindSpec ( findSpec ) where

import TestInit
import Data.List (sort)

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
      sort res @?= ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs",
                    "./FindSpec.hs", "./Help.hs", "./LiftedSpec.hs", "./MoveSpec.hs",
                    "./ReadFileSpec.hs", "./RmSpec.hs", "./TestInit.hs", "./TestMain.hs",
                    "./WhichSpec.hs", "./WriteSpec.hs", "./WriterSpec.hs", "./sleep.hs"]

    it "finds relative files" $ do
      res <- shelly $ cd "src" >> find "."
      sort res @?= ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs",
                    "./FindSpec.hs", "./Help.hs", "./LiftedSpec.hs", "./MoveSpec.hs",
                    "./ReadFileSpec.hs", "./RmSpec.hs", "./TestInit.hs", "./TestMain.hs",
                    "./WhichSpec.hs", "./WriteSpec.hs", "./WriterSpec.hs", "./sleep.hs"]

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
      sort res @?= [ "src/CopySpec.hs", "src/EnvSpec.hs", "src/FailureSpec.hs"
                   , "src/FindSpec.hs", "src/Help.hs", "src/LiftedSpec.hs"
                   , "src/MoveSpec.hs", "src/ReadFileSpec.hs", "src/RmSpec.hs"
                   , "src/TestInit.hs", "src/TestMain.hs", "src/WhichSpec.hs"
                   , "src/WriteSpec.hs", "src/WriterSpec.hs", "src/sleep.hs"]

    it "lists absolute files" $ do
      res <- shelly $ relPath "src" >>= find >>= mapM (relativeTo "src")
      sort res @?= [ "CopySpec.hs", "EnvSpec.hs", "FailureSpec.hs", "FindSpec.hs"
                   , "Help.hs", "LiftedSpec.hs", "MoveSpec.hs", "ReadFileSpec.hs"
                   , "RmSpec.hs", "TestInit.hs", "TestMain.hs", "WhichSpec.hs"
                   ,  "WriteSpec.hs", "WriterSpec.hs", "sleep.hs"]
