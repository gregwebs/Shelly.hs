{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
import Shelly
import Data.Text.Lazy as LT
default (LT.Text)

main :: IO ()
main =
  shelly $ do
    setStdin "in"
    echo "echo"
    setStdin "catted"
    run_ "cat" ["-"]
    res <- cmd "echo" "foo"
    cmd "echo" "bar" "baz"
    echo res
