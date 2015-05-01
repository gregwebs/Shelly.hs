module RunSpec ( runSpec ) where

import TestInit

import qualified Data.Text as T

runSpec :: Spec
runSpec = do
  describe "run" $ do
    it "simple command" $ do
      res <- shelly $ run "echo" [ "wibble" ]
      res @?= "wibble\n"

    it "with escaping" $ do
      res <- shelly $ run "echo" [ "*" ]
      res @?= "*\n"

    it "without escaping" $ do
      res <- shelly $ escaping False $ run "echo" [ "*" ]
      assert $ "README.md" `elem` T.words res
