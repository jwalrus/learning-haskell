-- Max Pair Monad example
-- Get Programming with Haskell (Kurt), Lesson 31

maxPairM' :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM' pair = pair >>= \(x,y) -> return $ max x y


maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM pair = do
    (x,y) <- pair
    return $ max x y
