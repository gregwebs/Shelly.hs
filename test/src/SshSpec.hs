module SshSpec ( sshSpec ) where

import TestInit

sshSpec :: Spec
sshSpec = do
  describe "sshCommand" $ do
    it "simple command" $ do
      let res = sshCommand [("wibble", [])] SeqSsh
      res @?= "'wibble'"

    it "space command" $ do
      let res = sshCommand [("to", ["outer space"])] SeqSsh
      res @?= "'to 'outer space''"
