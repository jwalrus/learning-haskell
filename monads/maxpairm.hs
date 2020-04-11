
maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM pair = do
    (x,y) <- pair
    return $ max x y
