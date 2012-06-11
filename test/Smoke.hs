{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import qualified Data.Text.Lazy as LT
default (LT.Text)

main :: IO ()
main =
  shelly $
    -- verbosely $
    do
    jobs 2 $ \job -> do
      _<- background job $ cmd "sleep" "2"
      echo "immediate"
      _<- background job $ cmd "sleep" "2"
      echo "immediate2"
      _<- background job $ cmd "sleep" "2"
      echo "blocked by background "
 
    echo "blocked by jobs"

    setStdin "in"
    setStdin "override stdin"
    run_ "cat" ["-"]

    recho <- cmd "echo" "cmd"
    _<-cmd "echo" "bar" "baz"
    echo recho

    (res :: Text) <- cmd "pwd"
    liftIO $ putStrLn $ show res
    inspect res

    inspect =<< (cmd "echo" "compose" :: ShIO Text)
    inspect =<< (cmd "pwd")

