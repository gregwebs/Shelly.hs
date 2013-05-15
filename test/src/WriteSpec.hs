{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module WriteSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit
import Test.Hspec

import Prelude hiding (FilePath)
import Shelly
import Data.Text.Lazy (Text)

default (Text)

main :: IO ()
main = hspec spec

creates_file :: FilePath -> (FilePath -> IO ()) -> IO ()
creates_file f action = do
  exists <- shelly $ test_e f
  when exists $ error "cleanup after yourself!"
  action f
  shelly $ rm f
  return ()


spec :: Spec
spec = do
  describe "writefile" $ do
    it "creates and overwites a file" $ creates_file "foo" $ \f -> do
      assert . (== "a") =<< (shelly $ writefile f "a" >> readfile f)
      assert . (== "b") =<< (shelly $ writefile f "b" >> readfile f)

  describe "appendfile" $ do
    it "creates and appends a file" $ creates_file "foo" $ \f -> do
      assert . (== "a")  =<< (shelly $ appendfile f "a" >> readfile f)
      assert . (== "ab") =<< (shelly $ appendfile f "b" >> readfile f)

  describe "touchfile" $ do
    it "creates and updates a file" $ creates_file "foo" $ \f -> do
      assert . (== "") =<< (shelly $ touchfile f >> readfile f)
      assert . (== "") =<< (shelly $ touchfile f >> readfile f)

      assert . (== "a") =<< (shelly $
        writefile f "a" >> touchfile f >> readfile f)
