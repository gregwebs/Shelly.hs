{-# Language OverloadedStrings, ExtendedDefaultRules #-}
import Prelude hiding (FilePath)
import Shelly
import Control.Monad (void)
import Data.Text (Text)

default (Text)

main :: IO ()
main = do
  let exDir = "./examples"
  void $ shelly $ do
    let strs = ["a", "b"] :: [String]
    let texts = ["a", "b"] :: [Text]
    let inferred = ["a", "b"]
    res <- cmd (exDir </> "drain.sh") strs texts inferred
    echo "haskell done"
    echo res
    cmd $ exDir </> "printer.sh"
