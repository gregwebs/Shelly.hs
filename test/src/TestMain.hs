
module Main where

import ReadFileSpec
import WhichSpec
import WriteSpec
import MoveSpec
import RmSpec
import FindSpec
import PrintCommandsFnSpec
import EnvSpec
import FailureSpec
import CopySpec
import LiftedSpec
import RunSpec
import ShowCommandSpec
import SshSpec
import PipeSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    readFileSpec
    whichSpec
    writeSpec
    moveSpec
    rmSpec
    findSpec
    printCommandsFnSpec
    envSpec
    failureSpec
    copySpec
    liftedSpec
    runSpec
    showCommandSpec
    sshSpec
    pipeSpec
