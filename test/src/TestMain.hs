module Main where

import ReadFileSpec
import WhichSpec
import WriteSpec
import WriterSpec
import MoveSpec
import RmSpec
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
    writerSpec
    moveSpec
    rmSpec
    findSpec
    envSpec
    failureSpec
    copySpec
    liftedSpec
