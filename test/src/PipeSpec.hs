module PipeSpec ( pipeSpec ) where

import TestInit

import Data.Text (Text)
import qualified Shelly.Pipe as P

pipeSpec :: Spec
pipeSpec = do
  describe "P.cmd" $ do
    let shouldBeTxt res t = res @?= [t :: Text]

    it "with Text" $ do
      res <- P.shelly $ P.cmd "echo" ("wibble" :: Text)
      res `shouldBeTxt` "wibble\n"

    it "with String" $ do
      res <- P.shelly $ P.cmd "echo" "wibble"
      res `shouldBeTxt` "wibble\n"

    it "with [Text]" $ do
      res <- P.shelly $ P.cmd "echo" (["wibble"] :: [Text])
      res `shouldBeTxt` "wibble\n"

    it "with [String]" $ do
      res <- P.shelly $ P.cmd "echo" ["wibble"]
      res `shouldBeTxt` "wibble\n"

    -- Check all two argument permutations (with replacement) of { Text, String, [Text], [String] }.
    it "with Text and Text" $ do
      res <- P.shelly $ P.cmd "echo" ("wibble" :: Text) ("wobble" :: Text)
      res `shouldBeTxt` "wibble wobble\n"

    it "with Text and String" $ do
      res <- P.shelly $ P.cmd "echo" ("wibble" :: Text) "wobble"
      res `shouldBeTxt` "wibble wobble\n"

    it "with Text and [Text]" $ do
      res <- P.shelly $ P.cmd "echo" ("wibble" :: Text) (["wobble", "wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with Text and [String]" $ do
      res <- P.shelly $ P.cmd "echo" ("wibble" :: Text) ["wobble", "wurble"]
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with String and Text" $ do
      res <- P.shelly $ P.cmd "echo" "wibble" ("wobble" :: Text)
      res `shouldBeTxt` "wibble wobble\n"

    it "with String and String" $ do
      res <- P.shelly $ P.cmd "echo" "wibble" "wobble"
      res `shouldBeTxt` "wibble wobble\n"

    it "with String and [Text]" $ do
      res <- P.shelly $ P.cmd "echo" "wibble" (["wobble", "wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and Text" $ do
      res <- P.shelly $ P.cmd "echo" (["wibble", "wobble"] :: [Text]) ("wurble" :: Text)
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and String" $ do
      res <- P.shelly $ P.cmd "echo" (["wibble", "wobble"] :: [Text]) "wurble"
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and [Text]" $ do
      res <- P.shelly $ P.cmd "echo" (["wibble", "wobble"] :: [Text]) (["wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [Text] and [String]" $ do
      res <- P.shelly $ P.cmd "echo" (["wibble", "wobble"] :: [Text]) ["wurble"]
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and Text " $ do
      res <- P.shelly $ P.cmd "echo" ["wibble", "wobble"] ("wurble" :: Text)
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and String " $ do
      res <- P.shelly $ P.cmd "echo" ["wibble", "wobble"] "wurble"
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and [Text] " $ do
      res <- P.shelly $ P.cmd "echo" ["wibble", "wobble"] (["wurble"] :: [Text])
      res `shouldBeTxt` "wibble wobble wurble\n"

    it "with [String] and [String] " $ do
      res <- P.shelly $ P.cmd "echo" ["wibble", "wobble"] ["wurble"]
      res `shouldBeTxt` "wibble wobble wurble\n"

    -- Check unit cases
    it "returns [()]" $ do
      res <- P.shelly $ P.cmd "echo" "wibble" "wobble"
      res @?= [()]

    it "works with underscore" $ do
      _ <- P.shelly $ P.cmd "echo" "wibble" "wobble"
      True `shouldBe` True
