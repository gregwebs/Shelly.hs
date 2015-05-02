{-# LANGUAGE CPP #-}

module ReadFileSpec (readFileSpec) where

import TestInit
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding ( FilePath, catch)
#else
import Prelude hiding ( FilePath)
#endif
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
 
