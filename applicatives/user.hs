-- creating data in a context
-- Get Programming with Haskell (Kurt), Lesson 28

data User = User 
    { name :: String
    , gamerId :: Int
    , score :: Int    
    } deriving Show

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Enter a username, gamerId, and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user


-- using applicatives to generate test data
testNames :: [String]
testNames = ["John Doe", "Robert '); DROP TABLES Students;--", "Chris Null"]

testIds :: [Int]
testIds = [1337, 0123, 99999]

testScores :: [Int]
testScores = [0, 1000, -9999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

allFmap :: (Applicative f) => (a -> b) -> f a -> f b
allFmap fn fa = fn <$> fa

example :: Int
example = (*) ((+) 2 4) 6

maybeExample :: Maybe Int
maybeExample = (*) <$> ((+) <$> pure 2 <*> pure 4) <*> pure 6

