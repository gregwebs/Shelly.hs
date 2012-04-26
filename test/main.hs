{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
    _<-cmd "echo" "bar" "baz"
    echo res
