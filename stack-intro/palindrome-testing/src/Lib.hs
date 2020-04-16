module Lib 
    ( isPalindrome
    , preProcess
    ) where

import Data.Text as T
import Data.Char (isPunctuation)

preProcess :: T.Text -> T.Text
preProcess text = T.filter (not . isPunctuation) text

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preProcess text
