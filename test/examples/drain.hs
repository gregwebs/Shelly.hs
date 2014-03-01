{-# Language OverloadedStrings, ExtendedDefaultRules #-}
import Prelude hiding (FilePath)
import Shelly
import Control.Monad (void)
import Data.Text (Text)

default (Text)

main :: IO ()
main = do
  let exDir = "examples"
  void $ shelly $ do
    res <- cmd $ exDir </> "drain.sh"
    echo "haskell done"
    echo res
    cmd $ exDir </> "printer.sh"
