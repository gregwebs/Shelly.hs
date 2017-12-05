
module Main where

import ReadFileSpec
import WhichSpec
import WriteSpec
import MoveSpec
import RmSpec
import FindSpec
import EnvSpec
import FailureSpec
import CopySpec
import LiftedSpec
import RunSpec
import SshSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    readFileSpec
    whichSpec
    writeSpec
    moveSpec
    rmSpec
    findSpec
    envSpec
    failureSpec
    copySpec
    liftedSpec
    runSpec
    sshSpec
