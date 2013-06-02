{-# LANGUAGE CPP #-}
{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}

module ReadFileSpec (readFileSpec) where

import Test.HUnit hiding (path)
import Test.Hspec
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding ( FilePath, catch)
#else
import Prelude hiding ( FilePath)
#endif
import Shelly
import qualified Data.ByteString as BS
import qualified Data.Text as T

readFileSpec :: Spec
readFileSpec = describe "file with invalid encoding" $ do
    it "readBinary" $ do
      res <- shelly $ readBinary "data/zshrc"
      assert (BS.length res > 0)
 
    it "readfile" $ do
      res <- shelly $ readfile "data/zshrc"
      assert (T.length res > 0)
 
