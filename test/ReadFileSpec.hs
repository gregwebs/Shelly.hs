{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module ReadFileSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit hiding (path)
import Test.Hspec
import Prelude hiding (catch, FilePath)
import Shelly
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as LT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "file with invalid encoding" $ do
    it "readBinary" $ do
      res <- shelly $ readBinary "test/data/zshrc"
      assert (BS.length res > 0)
 
    it "readfile" $ do
      res <- shelly $ readfile "test/data/zshrc"
      assert (LT.length res > 0)
 
