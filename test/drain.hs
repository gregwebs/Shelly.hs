{-# Language OverloadedStrings #-}
import Shellish

main :: IO ()
main =
  shellish $ do
    res <- run "test/drain.sh" []
    echo "haskell done"
    echo res
