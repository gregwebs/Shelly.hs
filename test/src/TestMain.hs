
module Main where

import ReadFileSpec
import WhichSpec
import WriteSpec
import MoveSpec
import FindSpec
import EnvSpec
import FailureSpec
import CopySpec
import LiftedSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    readFileSpec
    whichSpec
    writeSpec
    moveSpec
    findSpec
    envSpec
    failureSpec
    copySpec
    liftedSpec
