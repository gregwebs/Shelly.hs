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

  -- Bash-related commands
  describe "bash" $ do
    it "simple command" $ do
      res <- shelly $ bash "echo" [ "wibble" ]
      res @?= "wibble\n"

    it "without escaping" $ do
      res <- shelly $ escaping False $ bash "echo" [ "*" ]
      assert $ "README.md" `elem` T.words res

    it "with binary handle mode" $ do
      res <- shelly $ onCommandHandles (initOutputHandles (flip hSetBinaryMode True))
                    $ bash "cat" [ "test/data/nonascii.txt" ]
      res @?= "Selbstverst\228ndlich \252berraschend\n"

    {- This throws spurious errors on some systems
    it "can detect failing commands in pipes" $ do
      eCode <- shelly $ escaping False $ errExit False $ do
        bashPipeFail
          bash_ "echo" ["'foo'", "|", "ls", "\"eoueouoe\"", "2>/dev/null", "|", "echo", "'bar'" ]
        lastExitCode
      eCode `shouldSatisfy` (/= 0)
      -}

    it "preserve pipe behaviour" $ do
      (eCode, res) <- shelly $ escaping False $ errExit False $ do
        res <- bash "echo" [ "'foo'", "|", "echo", "'bar'" ]
        eCode <- lastExitCode
        return (eCode, res)
      res @?= "bar\n"
      eCode @?= 0
