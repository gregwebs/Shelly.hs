
module Main where

import ReadFileSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
    readFileSpec
