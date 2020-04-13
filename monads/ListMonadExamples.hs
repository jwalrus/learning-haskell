-- Use List Monads to Build Lists
-- Get Programming with Haskell (Kurt), Lesson 32
import Control.Monad -- guard lives here

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x) [1..n]

powersOfTwo :: Int -> [Int]
powersOfTwo n = [2^x | x <- [1..n]]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1..n]
    let powerOfTwo = 2^value
    let powerOfThree = 3^value
    return (powerOfTwo, powerOfThree)

powersOfTwoAndThreeComp :: Int -> [(Int, Int)]
powersOfTwoAndThreeComp n = [(pTwo, pThree) | x <- [1..n], let pTwo = 2^x, let pThree = 3^x]

-- all combinations of evens and odds up to `n'
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evens <- [2,4..n]
    odds <- [1,3..n]
    return (evens, odds)

allEvenOddsComp :: Int -> [(Int, Int)]
allEvenOddsComp n = [(even, odd) | even <- [2,4..n], odd <- [1,3..n]]

evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1..n]
    guard(even value)
    return value

evensComp :: Int -> [Int]
evensComp n = [ x | x <- [1..n], even x ]

