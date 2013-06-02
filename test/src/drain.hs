{-# Language OverloadedStrings #-}
import Shelly
import Control.Monad (void)

main :: IO ()
main = void $ shelly $ do
    res <- cmd "src/drain.sh"
    echo "haskell done"
    echo res
    cmd "src/printer.sh"
