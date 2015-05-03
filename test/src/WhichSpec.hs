module WhichSpec (whichSpec) where

import TestInit

whichSpec :: Spec
whichSpec = describe "which" $ do
    it "gives full path to cabal" $ do
      Just _ <- shelly $ which "find"
      assert True

    it "recognizes cabal as a path executable" $ do
      res <- shelly $ test_px "find"
      True @?= res
 
    it "cannot find missing exe" $ do
      Nothing <- shelly $ which "alskjdf;ashlva;ousnva;nj"
      assert True
