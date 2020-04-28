-- Haskell in Depth, Vitaly Bragilevsky
-- Chapter 2

{-# LANGUAGE DeriveAnyClass #-}
import Data.List (nub, sort)


class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d | d == minBound = maxBound
            | otherwise = pred d

    csucc :: a -> a
    csucc d | d == maxBound = minBound
            | otherwise = succ d

class (Enum a, Bounded a) => BoundedEnum a where
    range :: [a]
    range = enumFrom minBound

data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, BoundedEnum, CyclicEnum, Show)

data Turn = TNone | TLeft | TRight | TAround
    deriving (Eq, Enum, Bounded, BoundedEnum, Ord, Show)

orient :: Turn -> Direction -> Direction
orient TNone = id 
orient TLeft = cpred
orient TRight = csucc
orient TAround = cpred . cpred


findTurn :: Direction -> Direction -> Turn
findTurn d1 d2 = head $ filter (\t -> orient t d1 == d2) range

test :: Bool
test = sort (nub [ findTurn d1 d2 | d1 <- range, d2 <- range ]) == range
