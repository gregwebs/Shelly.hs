module RunSpec ( runSpec ) where

import TestInit

import qualified Data.Text as T
import Data.Text (Text)
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
      if isWindows
        then res @?= "*\n"
        else assert $ "README.md" `elem` T.words res

    it "with binary handle mode" $ do
      res <- shelly $ onCommandHandles (initOutputHandles (flip hSetBinaryMode True))
                    $ run "cat" [ "test/data/nonascii.txt" ]
      if isWindows
        then res @?= "Selbstverst\228ndlich \252berraschend\r\n"
        else res @?= "Selbstverst\228ndlich \252berraschend\n"
    unless isWindows $ do
      it "script at $PWD" $ do
        res <- shelly $ do
          run_ "chmod" ["+x", "test/data/hello.sh"]
          run "./test/data/hello.sh" []
        res @?= "Hello!\n"

  describe "cmd" $ do
    let shouldBeTxt res t = res @?= (t :: Text)

    it "with Text" $ do
      res <- shelly $ cmd "echo" ("wibble" :: Text)
      res `shouldBeTxt` "wibble\n"

    it "with String" $ do
      res <- shelly $ cmd "echo" "wibble"
      res `shouldBeTxt` "wibble\n"

    it "with [Text]" $ do
      res <- shelly $ cmd "echo" (["wibble"] :: [Text])
      res `shouldBeTxt` "wibble\n"

    it "with [String]" $ do
      res <- shelly $ cmd "echo" ["wibble"]
      res `shouldBeTxt` "wibble\n"

    -- Check all two argument permutations (with replacement) of { Text, String, [Text], [String] }.
    it "with Text and Text" $ do
      res <- shelly $ cmd "echo" ("wibble" :: Text) ("wobble" :: Text)
      res `shouldBeTxt` "wibble wobble\n"

    it "with Text and String" $ do
      res <- shelly $ cmd "echo" ("wibble" :: Text) "wobble"
      res `shouldBeTxt` "wibble wobble\n"

    it "with Text and [Text]" $ do
      res <- shelly $ cmd "echo" ("wibble" :: Text) (["wobble", "wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with Text and [String]" $ do
      res <- shelly $ cmd "echo" ("wibble" :: Text) ["wobble", "wurble"]
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with String and Text" $ do
      res <- shelly $ cmd "echo" "wibble" ("wobble" :: Text)
      res `shouldBeTxt` "wibble wobble\n"

    it "with String and String" $ do
      res <- shelly $ cmd "echo" "wibble" "wobble"
      res `shouldBeTxt` "wibble wobble\n"

    it "with String and [Text]" $ do
      res <- shelly $ cmd "echo" "wibble" (["wobble", "wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and Text" $ do
      res <- shelly $ cmd "echo" (["wibble", "wobble"] :: [Text]) ("wurble" :: Text)
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and String" $ do
      res <- shelly $ cmd "echo" (["wibble", "wobble"] :: [Text]) "wurble"
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and [Text]" $ do
      res <- shelly $ cmd "echo" (["wibble", "wobble"] :: [Text]) (["wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and [String]" $ do
      res <- shelly $ cmd "echo" (["wibble", "wobble"] :: [Text]) ["wurble"]
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and Text " $ do
      res <- shelly $ cmd "echo" ["wibble", "wobble"] ("wurble" :: Text)
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and String " $ do
      res <- shelly $ cmd "echo" ["wibble", "wobble"] "wurble"
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and [Text] " $ do
      res <- shelly $ cmd "echo" ["wibble", "wobble"] (["wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and [String] " $ do
      res <- shelly $ cmd "echo" ["wibble", "wobble"] ["wurble"]
      res `shouldBeTxt` "wibble wobble wurble\n"

    -- Check unit cases
    it "returns Unit" $ do
      res <- shelly $ cmd "echo" "wibble" "wobble"
      res @?= ()

    it "works with underscore" $ do
      _ <- shelly $ cmd "echo" "wibble" "wobble"
      True `shouldBe` True

    -- This should now compile without a warning since ghc should infer Sh () instead of Sh Text.
    it "defaults to Unit" $ do
      shelly $ cmd "echo" "wibble" "wobble"
      True `shouldBe` True

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
      if isWindows
        then res @?= "Selbstverst\228ndlich \252berraschend\r\n"
        else res @?= "Selbstverst\228ndlich \252berraschend\n"

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
        res <-
          if isWindows
            then bash "echo" [ "foo", "|", "echo", "bar" ]
            else bash "echo" [ "'foo'", "|", "echo", "'bar'" ]
        eCode <- lastExitCode
        return (eCode, res)
      if isWindows
        then res @?= "bar'\n"
        else res @?= "bar\n"
      eCode @?= 0
