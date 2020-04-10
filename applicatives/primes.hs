
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where twoThroughN = [2..n]
          composites = (*) <$> twoThroughN <*> twoThroughN -- list as context to calculate composite numbers
          isNotComposite = not . (`elem` composites)