{-# LANGUAGE CPP #-}

module ReadFileSpec (readFileSpec) where

import TestInit
import qualified Data.ByteString as BS
import qualified Data.Text as T

readFileSpec :: Spec
readFileSpec = describe "file with invalid encoding" $ do
    it "readBinary" $ do
      res <- shelly $ readBinary "test/data/zshrc"
      assert (BS.length res > 0)

    it "readfile" $ do
      res <- shelly $ readfile "test/data/zshrc"
      assert (T.length res > 0)
