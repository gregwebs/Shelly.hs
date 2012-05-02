{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Data.Text.Lazy as LT
default (LT.Text)

main :: IO ()
main =
  shelly $ do
    jobs 2 $ \job -> do
      background job $ cmd "sleep" "2"
      echo "yawn"
      background job $ cmd "sleep" "2"
      echo "tired"
      background job $ cmd "sleep" "2"
      echo "zzzz"
 
    setStdin "in"
    echo "echo"
    setStdin "catted"
    run_ "cat" ["-"]
    res <- cmd "echo" "foo"
    _<-cmd "echo" "bar" "baz"
    echo res
