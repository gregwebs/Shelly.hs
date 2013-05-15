{-# Language OverloadedStrings #-}
import Shelly

main :: IO ()
main =
  shelly $ do
    res <- run "test/drain.sh" []
    echo "haskell done"
    echo res
