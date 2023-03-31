module ShowCommandSpec (showCommandSpec) where

import TestInit

showCommandSpec :: Spec
showCommandSpec = do
  describe "show_command" $ do
    it "preserves the empty string" $ do
      show_command "echo" [""] @?= "echo \"\""

    it "does not quote arguments that do not contain special characters" $ do
      show_command "echo" ["a1~!@#%^-_+=:,.?/"] @?= "echo a1~!@#%^-_+=:,.?/"

    it "quotes whitespace" $ do
      show_command "echo" [" "] @?= "echo \" \""
      show_command "echo" ["\t"] @?= "echo \"\t\""
      show_command "echo" ["\r"] @?= "echo \"\r\""
      show_command "echo" ["\n"] @?= "echo \"\n\""


    it "quotes arguments that contain special characters" $ do
      show_command "echo" ["'"] @?= "echo \"'\""
      show_command "echo" ["&"] @?= "echo \"&\""
      show_command "echo" ["|"] @?= "echo \"|\""
      show_command "echo" [";"] @?= "echo \";\""
      show_command "echo" ["("] @?= "echo \"(\""
      show_command "echo" [")"] @?= "echo \")\""
      show_command "echo" ["{"] @?= "echo \"{\""
      show_command "echo" ["}"] @?= "echo \"}\""
      show_command "echo" ["<"] @?= "echo \"<\""
      show_command "echo" [">"] @?= "echo \">\""

    it "escapes the few special characters that must be escaped even in quotes" $ do
      show_command "echo" ["\""] @?= "echo \"\\\"\""
      show_command "echo" ["\\"] @?= "echo \"\\\\\""
      show_command "echo" ["$"] @?= "echo \"\\$\""
      show_command "echo" ["`"] @?= "echo \"\\`\""
