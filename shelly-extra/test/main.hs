{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Shelly.Background
import Data.Text (Text, stripEnd)
import Test.HUnit
default (Text)

shIOUnit :: Sh ()
shIOUnit = do
  cmd "pwd"
  chdir ".." $
    cmd "pwd"

main :: IO ()
main = do
  -- stdin
  shelly $ do
    setStdin "in"
    let overStdin = "override stdin"
    setStdin overStdin
    over <- run "cat" ["-"]
    liftIO $ stripEnd over @?= overStdin

  -- various uses of cmd
  shelly $ do
    recho <- cmd "echo" "cmd"
    _<-cmd "echo" "bar" "baz"
    echo recho

    (res :: Text) <- cmd "pwd"
    liftIO $ print res
    inspect res

    inspect =<< (cmd "echo" "compose" :: Sh Text)
    inspect =<< cmd "pwd"

    -- this somehow forces more evaluation
    shIOUnit

  -- waiting on background jobs
  shelly $ do
    jobs 2 $ \job -> do
      _<- background job $ cmd "sleep" "2"
      echo "immediate"
      _<- background job $ cmd "sleep" "2"
      echo "immediate2"
      _<- background job $ cmd "sleep" "2"
      echo "blocked by background "

    echo "blocked by jobs"

