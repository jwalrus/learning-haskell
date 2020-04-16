import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char (isPunctuation)
import Data.Text as T

-- PROPERTIES
prop_punctuationInvariant text = preProcess text == preProcess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)


-- entry point
main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    quickCheck prop_reverseInvariant
    putStrLn "done!"
