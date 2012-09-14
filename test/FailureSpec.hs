{-# Language OverloadedStrings #-}
{-# Language ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module FailureSpec (main, spec) where

import Test.Hspec.HUnit ()
import Test.HUnit
import Test.Hspec.Monadic

import Shelly

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  let shellyNoDir = shellyOpts defaultOpts { failToDir = False }
  describe "failure set to stderr" $
    it "writes a failure message to stderr" $ do
      hasD <- shelly $ do
        test_d ".shelly" >>= liftIO . assert . not
        _<- shellyNoDir (error "bam!") `catchany_sh` \_ -> return ()
        test_d ".shelly"
      assert $ not hasD

  describe "failure set to directory" $
    it "writes a failure message to a .shelly directory" $ do
      hasD <- shelly $ do
        test_d ".shelly" >>= liftIO . assert . not
        _<- shellyNoDir (error "bam!") `catchany_sh` \_ -> return ()
        test_d ".shelly"
      assert hasD
