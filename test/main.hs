{- # OPTIONS_GHC -F -pgmF hspec-discover -optF --nested #-}
{-import qualified CopySpec-}
{-main = CopySpec.main-}
import qualified FailureSpec
main :: IO ()
main = FailureSpec.main
