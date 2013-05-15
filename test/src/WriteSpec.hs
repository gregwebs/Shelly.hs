{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module WriteSpec ( writeSpec ) where

import Test.Hspec.HUnit ()
import Test.HUnit
import Test.Hspec

import Prelude hiding (FilePath)
import Shelly
import Data.Text (Text)

default (Text)

createsFile :: FilePath -> (FilePath -> IO ()) -> IO ()
createsFile f action = do
  exists <- shelly $ test_e f
  when exists $ error "cleanup after yourself!"
  action f
  shelly $ rm f
  return ()


writeSpec :: Spec
writeSpec = do
  describe "writefile" $
    it "creates and overwites a file" $ createsFile "foo" $ \f -> do
      assert . (== "a") =<< (shelly $ writefile f "a" >> readfile f)
      assert . (== "b") =<< (shelly $ writefile f "b" >> readfile f)

  describe "appendfile" $
    it "creates and appends a file" $ createsFile "foo" $ \f -> do
      assert . (== "a")  =<< (shelly $ appendfile f "a" >> readfile f)
      assert . (== "ab") =<< (shelly $ appendfile f "b" >> readfile f)

  describe "touchfile" $
    it "creates and updates a file" $ createsFile "foo" $ \f -> do
      assert . (== "") =<< (shelly $ touchfile f >> readfile f)
      assert . (== "") =<< (shelly $ touchfile f >> readfile f)

      assert . (== "a") =<< (shelly $
        writefile f "a" >> touchfile f >> readfile f)
