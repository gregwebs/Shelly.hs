{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import System.IO
import Data.Text as T
default (T.Text)

main :: IO ()
main =  do
  hSetBuffering stdout LineBuffering
  shelly $ verbosely $ do
    host <- run "uname" ["-n"]
    if T.stripEnd host == "local-machine"
      then do d <- cmd "date"
              c <- escaping False $ cmd "git" "log -1 | head -1 | awk '{print $2}'"
              appendfile "log/deploy.log" $ T.intercalate " - " [T.stripEnd d, c]
              uploads "my-server:/remote/path/" ["deploy"]
              sshPairs_ "my-server" [("cd", ["/remote/path"]), ("./deploy", [])]
      else do
            cmd "./script/angel"

-- same path on remote host
-- will create directories
uploads :: Text -> [Text] -> Sh ()
uploads remote locals = rsync $ ["--relative"] ++ locals ++ [remote]

rsync :: [Text] -> Sh ()
rsync args = run_ "rsync" $ ["--delete", "-avz", "--no-g"] ++ args
