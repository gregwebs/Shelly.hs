module FailureSpec ( failureSpec ) where

import TestInit

failureSpec :: Spec
failureSpec = do
  let discardException action = shellyFailDir $ catchany_sh action (\_ -> return ())

  describe "failure set to stderr" $
    it "writes a failure message to stderr" $ do
      shellyFailDir $ discardException $
        liftIO $ shelly $ do
          test_d ".shelly" >>= liftIO . assert . not
          echo "testing"
          error "bam!"
      assert . not =<< shellyFailDir (test_d ".shelly")

  describe "failure set to directory" $
    it "writes a failure message to a .shelly directory" $ do
      shellyFailDir $ discardException $
        shellyFailDir $ do
          test_d ".shelly" >>= liftIO . assert . not
          echo "testing"
          error "bam!"
      assert =<< shellyFailDir ( do
          exists <- test_d ".shelly"
          rm_rf ".shelly"
          return exists
        )
