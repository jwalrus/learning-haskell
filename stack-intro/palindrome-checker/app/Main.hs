module Main where

import Lib
import Data.Text as T
import Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Enter a word, and I will tell you if it is a palindrome!"
    text <- TIO.getLine
    let response = if isPalindrome text
                   then "It is!"
                   else "It is not!"                       
    TIO.putStrLn response
