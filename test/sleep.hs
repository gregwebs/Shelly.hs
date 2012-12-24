{-# Language OverloadedStrings #-}
import Shelly

main :: IO ()
main =
  shelly $ do
    echo "sleeping"
    run "sleep" ["5"]
    echo "all done"
