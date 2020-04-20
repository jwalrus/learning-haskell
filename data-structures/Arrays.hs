import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import Control.Monad
import Control.Monad.ST

-- NOTE: in GHCi, you can use `:set +s` to turn on execution time and memory measurements
-- > :set +s
-- > [1..1000000] !! 999999
--   1000000
--   (0.05 secs, 460,064 bytes)

zeroIndexedArray :: UArray Int Bool
zeroIndexedArray = array (0,9) [(3,True),(7,True)]

oneIndexedArray :: UArray Int Bool
oneIndexedArray = array (1,10) [(4,True),(8,True)]

twoIndexedArray :: UArray Int Bool
twoIndexedArray = array (2,11) [(5,True),(9,True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) $ zip [0..3] $ cycle [0]

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5),(3,6)] -- functional array update

-- add two beans to each bucket
addTwoBeans :: UArray Int Int -> UArray Int Int
addTwoBeans arr = accum (+) arr $ zip [start .. finish] $ cycle [2]
    where (start,finish) = bounds arr

-- MUTATING STATE (?!?!)
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end = length vals - 1
    stArray <- newArray (0,end) 0
    forM_ [0..end] $ \i -> do
        let val = vals !! i
        writeArray stArray i val
    return stArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

-- aside on ST Type
-- ST generalizes the behavior of STUArray
swapST :: (Int,Int) -> (Int,Int)
swapST (x,y) = runST $ do
    x' <- newSTRef x
    y' <- newSTRef y
    writeSTRef x' y
    writeSTRef y' x
    xfinal <- readSTRef x'
    yfinal <- readSTRef y'
    return (xfinal, yfinal)

-- BUBBLE SORT
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let (_,end) = bounds myArray
    forM_ [1..end] $ \i -> do
        forM_ [0..(end-i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j+1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j+1) nextVal
    return stArray
