module FindSpec ( findSpec ) where

import TestInit
import Data.List (sort)
import System.Directory (createDirectoryIfMissing)
import System.PosixCompat.Files (createSymbolicLink, fileExist)
import qualified System.FilePath as SF

createSymlinkForTest :: IO ()
createSymlinkForTest = do
  createDirectoryIfMissing False symDir
  fexist <- fileExist (symDir SF.</> "symlinked_dir")
  if fexist
    then return ()
    else createSymbolicLink
           (".." SF.</> "symlinked_dir")
           (symDir SF.</> "symlinked_dir")
  where
    rootDir = "test" SF.</> "data"
    symDir = rootDir SF.</> "dir"

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
      res <- shelly $ cd "test/src" >> ls "."
      sort res @?= ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs",
                    "./FindSpec.hs", "./Help.hs", "./LiftedSpec.hs", "./LogWithSpec.hs", "./MoveSpec.hs",
                    "./ReadFileSpec.hs", "./RmSpec.hs", "./RunSpec.hs", "./SshSpec.hs",
                    "./TestInit.hs", "./TestMain.hs",
                    "./WhichSpec.hs", "./WriteSpec.hs", "./sleep.hs"]

    it "finds relative files" $ do
      res <- shelly $ cd "test/src" >> find "."
      sort res @?= ["./CopySpec.hs", "./EnvSpec.hs", "./FailureSpec.hs",
                    "./FindSpec.hs", "./Help.hs", "./LiftedSpec.hs", "./LogWithSpec.hs", "./MoveSpec.hs",
                    "./ReadFileSpec.hs", "./RmSpec.hs", "./RunSpec.hs", "./SshSpec.hs",
                    "./TestInit.hs", "./TestMain.hs",
                    "./WhichSpec.hs", "./WriteSpec.hs", "./sleep.hs"]

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
      res <- shelly $ find "test/src"
      sort res @?= ["test/src/CopySpec.hs", "test/src/EnvSpec.hs", "test/src/FailureSpec.hs",
                    "test/src/FindSpec.hs", "test/src/Help.hs", "test/src/LiftedSpec.hs",
                    "test/src/LogWithSpec.hs", "test/src/MoveSpec.hs", "test/src/ReadFileSpec.hs",
                    "test/src/RmSpec.hs", "test/src/RunSpec.hs", "test/src/SshSpec.hs",
                    "test/src/TestInit.hs", "test/src/TestMain.hs", "test/src/WhichSpec.hs", "test/src/WriteSpec.hs",
                    "test/src/sleep.hs"]

    it "lists absolute files" $ do
      res <- shelly $ relPath "test/src" >>= find >>= mapM (relativeTo "test/src")
      sort res @?= ["CopySpec.hs", "EnvSpec.hs", "FailureSpec.hs", "FindSpec.hs",
                    "Help.hs", "LiftedSpec.hs", "LogWithSpec.hs", "MoveSpec.hs",
                    "ReadFileSpec.hs", "RmSpec.hs", "RunSpec.hs", "SshSpec.hs",
                    "TestInit.hs", "TestMain.hs",
                    "WhichSpec.hs", "WriteSpec.hs", "sleep.hs"]

    before createSymlinkForTest $ do
      it "follow symlinks" $
         do res <-
              shelly $
              followSymlink True $
              relPath "test/data" >>= find >>= mapM (relativeTo "test/data")
            sort res @?=
              [ "dir"
              , "nonascii.txt"
              , "symlinked_dir"
              , "zshrc"
              , "dir/symlinked_dir"
              , "dir/symlinked_dir/hoge_file"
              , "symlinked_dir/hoge_file"
              ]
      it "not follow symlinks" $
         do res <-
              shelly $
              followSymlink False $
              relPath "test/data" >>= find >>= mapM (relativeTo "test/data")
            sort res @?=
              [ "dir"
              , "nonascii.txt"
              , "symlinked_dir"
              , "zshrc"
              , "dir/symlinked_dir"
              , "symlinked_dir/hoge_file"
              ]

