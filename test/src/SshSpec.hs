module SshSpec ( sshSpec ) where

import TestInit

sshSpec :: Spec
sshSpec = do
  describe "sshCommandText" $ do
    describe "escaping off" $ do
        it "simple command" $ do
          let res = sshCommandText [("wibble", [])] False SeqSsh
          res @?= "\"wibble\""

        it "space command" $ do
          let res = sshCommandText [("to", ["outer space"])] False SeqSsh
          res @?= "\"to 'outer space'\""

        it "multiple space commands" $ do
          let res = sshCommandText [("to", ["outer space"]), ("and", ["back again"])] False SeqSsh
          res @?= "\"to 'outer space' && and 'back again'\""

    describe "escaping on" $ do
        it "simple command" $ do
          let res = sshCommandText [("wibble", [])] True SeqSsh
          res @?= "\"'wibble'\""

        it "space command" $ do
          let res = sshCommandText [("to", ["outer space"])] True SeqSsh
          res @?= "\"'to' 'outer space'\""

        it "multiple space commands" $ do
          let res = sshCommandText [("to", ["outer space"]), ("and", ["back again"])] True SeqSsh
          res @?= "\"'to' 'outer space' && 'and' 'back again'\""
