{-# Language OverloadedStrings #-}
import Shelly

main :: IO ()
main =
  shelly $ do
    setStdin "in"
    echo "echo"
    setStdin "catted"
    run_ "cat" ["-"]
