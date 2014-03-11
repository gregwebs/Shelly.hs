{-# Language OverloadedStrings, ExtendedDefaultRules #-}
import Shelly
-- This test runs, but causes this error to show up:
-- Exception: cannot access an inherited pipe
main = shelly $
  runHandles "bash" ["examples/test.sh"] handles doNothing
  where handles = [InHandle Inherit, OutHandle Inherit, ErrorHandle Inherit]
        doNothing _ _ _ = return ""
