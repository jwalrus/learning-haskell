-- multi argument function in io
-- Get Programming with Haskell (Kurt), Lesson 28

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree v1 v2 v3 = min v1 $ min v2 v3

readInt :: IO Int 
readInt = read <$> getLine

minOfThreeInts :: IO Int
minOfThreeInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
   putStrLn "Enter three numbers"
   minInt <- minOfInts
   putStrLn (show minInt ++ " is the smallest")