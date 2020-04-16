-- Semigroup of Colors
-- Get Programming with Haskell (Kurt), Lesson 17

data Color = Red
    | Yellow
    | Blue
    | Green
    | Purple
    | Orange
    | Brown deriving (Eq, Show)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = b
             | all (`elem` [Red, Blue, Purple]) [a,b] = Purple
             | all (`elem` [Yellow, Blue, Green]) [a,b] = Green
             | all (`elem` [Yellow, Red, Orange]) [a,b] = Orange
             | otherwise = Brown 

