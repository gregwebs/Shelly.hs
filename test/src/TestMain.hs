
module Main where

import ReadFileSpec
import WriteSpec
import MoveSpec
import FindSpec
import EnvSpec
import FailureSpec
import CopySpec
import RunSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    readFileSpec
    writeSpec
    moveSpec
    findSpec
    envSpec
    failureSpec
    copySpec
    runSpec
