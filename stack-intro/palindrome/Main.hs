-- Palindrome
-- Get Programming with Haskell (Kurt), Lesson 34

module Main where

import qualified Palindrome

main :: IO ()
main = do
    putStrLn "Enter a word, and I will tell you whether it is a palindrome!"
    word <- getLine
    let response = if Palindrome.isPalindrome word
                   then "it is!"
                   else "it is not!"
    putStrLn response    
