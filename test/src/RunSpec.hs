{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module RunSpec (runSpec) where

import Test.HUnit hiding (path)
import Test.Hspec
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import Prelude hiding ( FilePath, catch )
#else
import Prelude hiding ( FilePath )
#endif
import Shelly

runSpec :: Spec
runSpec = do
    describe "run should" $ do
      it "return the full stdout" $ do
        res <- shelly $ escaping False $ run "ls" ["*.hs"]
        res @?= "Setup.hs\n"
