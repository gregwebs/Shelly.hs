{-# Language OverloadedStrings #-}
import Shelly

main :: IO ()
main =
  shelly $ do
    echo "sleeping"
    sleep 5
    echo "all done"
