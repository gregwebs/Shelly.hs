{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module CopySpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit hiding (path)
import Test.Hspec.Monadic
import Prelude hiding (catch, FilePath)
import Shelly
import Control.Monad (forM_)
import System.IO.Error

main :: IO ()
main = hspecX spec

with_dir :: FilePath -> Sh a -> Sh a
with_dir d action =
  mkdir_p d >> (action `finally_sh` rm_rf d)

within_dir :: FilePath -> Sh a -> Sh a
within_dir d action =
  with_dir d $ chdir d action

spec :: Specs
spec = do
  let b = "b" 
  let c = "c"
  describe "cp file" $ do
    it "cp to same dir" $ do
      forM_ [cp, cp_r] $ \copier -> do
        res <- shelly $ do
          within_dir "test/a" $ do
            writefile b "testing"
            copier b c
            readfile c
        res @?= "testing"

    it "cp to other dir" $ do
      forM_ [cp, cp_r] $ \copier -> do
        res <- shelly $ do
          within_dir "test/a" $ do
            writefile b "testing"
            mkdir c
            copier b c
            readfile "c/b"
        res @?= "testing"

  describe "cp dir" $ do
    it "creates a dir at the same level" $ do
      res <- shelly $ do
        within_dir "test/a" $ do
          mkdir b
          writefile "b/d" ""
          cp_r b c
          cIsDir <- test_d c
          liftIO $ assert $ cIsDir
          test_f "c/d"
      assert res

    it "creates a to dir at a different level" $ do
      res <- shelly $ do
        within_dir "test/a" $ do
          mkdir b
          mkdir c
          writefile "b/d" ""
          cp_r b $ c</>b
          cIsDir <- test_d c
          liftIO $ assert $ cIsDir
          bIsDir <- test_d $ c</>b
          liftIO $ assert $ bIsDir
          test_f "c/b/d"
      assert res

    it "copies the same dir" $ do
      shelly $ do
        within_dir "test/a" $ do
          mkdir b
          writefile "b/d" ""
          cp_r b b `catch_sh` (\e -> liftIO $ assert $ isUserError e)
      assert True
