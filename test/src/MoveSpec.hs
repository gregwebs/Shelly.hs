module MoveSpec (moveSpec) where

import TestInit
import Help

moveSpec :: Spec
moveSpec = do
    let b = "b" 
    let c = "c"
    describe "mv file" $ do
      it "to same dir" $ do
        res <- shelly $
          within_dir "test/a" $ do
            writefile b "testing"
            mv b c
            readfile c
        res @?= "testing"
    
      it "to other dir" $ do
        res <- shelly $
          within_dir "test/a" $ do
            writefile b "testing"
            mkdir c
            mv b c
            readfile "c/b"
        res @?= "testing"
    
    describe "mv dir" $ do
      it "to dir does not exist: create the to dir" $ do
        res <- shelly $
          within_dir "test/a" $ do
            mkdir b
            writefile "b/d" ""
            mv b c
            cIsDir <- test_d c
            liftIO $ assert cIsDir
            test_f "c/d"
        assert res
    
      it "to dir exists: creates a nested directory, full to path given" $ do
        res <- shelly $
          within_dir "test/a" $ do
            mkdir b
            mkdir c
            writefile "b/d" ""
            mv b $ c</>b
            cIsDir <- test_d c
            liftIO $ assert cIsDir
            bIsDir <- test_d $ c</>b
            liftIO $ assert bIsDir
            test_f "c/b/d"
        assert res
    
      it "to dir exists: creates a nested directory, partial to path given" $ do
        res <- shelly $
          within_dir "test/a" $ do
            mkdir b
            mkdir c
            writefile "b/d" ""
            mv b $ c
            cIsDir <- test_d c
            liftIO $ assert cIsDir
            bIsDir <- test_d $ c</>b
            liftIO $ assert bIsDir
            test_f "c/b/d"
        assert res
    
      {-
      it "mv the same dir" $ do
        shelly $ do
          within_dir "test/a" $ do
            mkdir b
            writefile "b/d" ""
            mv b b `catch_sh` (\e -> liftIO $ assert $ isUserError e)
        assert True
        -}
