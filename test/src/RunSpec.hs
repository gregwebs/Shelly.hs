module RunSpec ( runSpec ) where

import TestInit

import qualified Data.Text as T
import System.IO

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

    it "with binary handle mode" $ do
      res <- shelly $ onCommandHandles (initOutputHandles (flip hSetBinaryMode True))
                    $ run "cat" [ "test/data/nonascii.txt" ]
      res @?= "Selbstverst\228ndlich \252berraschend\n"
