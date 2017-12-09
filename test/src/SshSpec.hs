{-# LANGUAGE OverloadedStrings #-}
module SshSpec ( sshSpec ) where

import TestInit
import qualified Data.Text as T

sshSpec :: Spec
sshSpec = do
  let q = "'" -- a single quote
  let qq = "'\\''" -- quote of a single quote
  let qqq = T.concat [qq, "\\", qq, qq] -- quote of qq
  describe "sshCommandText" $ do
    it "simple command" $ do
      let res = sshCommandText [("wibble", [])] SeqSsh
      res @?= T.concat [q, qq, "wibble", qq, q]

    it "space command" $ do
      let res = sshCommandText [("to", ["outer space"])] SeqSsh
      res @?= T.concat [q, qq, "to", qq, " ", qq, "outer space", qq ,q]

    it "multiple space commands" $ do
      let res = sshCommandText [("to", ["outer space"]), ("and", ["back again"])] SeqSsh
      res @?= T.concat
                [ q, qq, "to", qq, " ", qq, "outer space", qq
                , " && "
                , qq, "and", qq, " ", qq, "back again", qq, q
                ]

    it "commands with quotes and spaces" $ do
      let res = sshCommandText [ ("echo", ["Godfater's brother, Tom says: \"huh??\""])
                               , ("foo", ["--dir", "Tom's father/"])] SeqSsh
      res @?= T.concat
                [ q, qq, "echo", qq, " "
                , qq, "Godfater", qqq, "s brother, Tom says: \"huh??\"", qq
                , " && "
                , qq, "foo", qq, " "
                , qq, "--dir", qq, " "
                , qq, "Tom", qqq, "s father/", qq, q
                ]
