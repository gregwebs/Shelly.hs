
module Main where

import ReadFileSpec
import WriteSpec
import MoveSpec
import FindSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
    readFileSpec
    writeSpec
    moveSpec
    findSpec
