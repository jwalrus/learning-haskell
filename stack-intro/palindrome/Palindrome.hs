-- palindrome
-- Get Programming with Haskell (Kurt), Lesson 34

module Palindrome
    ( isPalindrome
    , preProcess
    ) where

import Data.Char (toLower, isSpace, isPunctuation)

stripWhiteSpace :: String -> String
stripWhiteSpace str = filter (not . isSpace) str

stripPunctuation :: String -> String
stripPunctuation str = filter (not . isPunctuation) str

toLowerCase :: String -> String
toLowerCase str = map toLower str

preProcess :: String -> String
preProcess = toLowerCase . stripPunctuation . stripWhiteSpace

isPalindrome :: String -> Bool
isPalindrome str = cleanStr == reverse cleanStr
    where cleanStr = preProcess str
