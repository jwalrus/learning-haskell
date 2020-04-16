module Lib
    ( isPalindrome
    ) where

import qualified Data.Text as T
import Data.Char (toLower, isPunctuation, isSpace)

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.map toLower text

preProcess :: T.Text -> T.Text
preProcess = toLowerCase . stripPunctuation . stripWhiteSpace

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preProcess text
