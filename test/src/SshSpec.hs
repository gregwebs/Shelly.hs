module SshSpec ( sshSpec ) where

import TestInit

sshSpec :: Spec
sshSpec = do
  describe "sshCommandText" $ do
    it "simple command" $ do
      let res = sshCommandText [("wibble", [])] SeqSsh
      res @?= "\"wibble\""

    it "space command" $ do
      let res = sshCommandText [("to", ["outer space"])] SeqSsh
      res @?= "\"to 'outer space'\""

    it "multiple space commands" $ do
      let res = sshCommandText [("to", ["outer space"]), ("and", ["back again"])] SeqSsh
      res @?= "\"to 'outer space' && and 'back again'\""
