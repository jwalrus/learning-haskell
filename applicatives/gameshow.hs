-- game show
-- Get Programming with Haskell (Kurt), Lesson 29

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

totalPrize :: [Int] 
totalPrize = (+) <$> doorPrize <*> boxPrize