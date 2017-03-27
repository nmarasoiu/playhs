import Test.QuickCheck
import Control.Monad
import Data.Foldable

data Tree a = Empty | Node a [Tree a]
              deriving (Show, Eq)
-- a tree is balanced iff the height of all the leaves are a set of 1 value or 2 adjacent values
isBalanced :: Tree a -> Bool
isBalanced t = maxH-minH <=1 where (minH, maxH) = heightRange t

heightRange :: Tree a -> (Integer, Integer)
heightRange Empty = (0,0)
heightRange (Node val []) = (1,1)
heightRange (Node val trees@(firstChild:otherChildren)) = ((foldl1 min minHeights)+1, (foldl1 max maxHeights)+1) where
    pairList = [heightRange tree | tree <- trees]
    minHeights = [minH | (minH, maxH) <- pairList]
    maxHeights = [maxH | (minH, maxH) <- pairList]

mapTuple :: (a->b) -> (a,a) -> (b,b)
mapTuple f (x,y) = (f x, f y)

unionRange :: Ord a => (a,a) -> (a,a) -> (a,a)
unionRange (aa, ab) (ba, bb) = ((min aa ba), (max ab bb))

leaf a = Node a []
children :: Tree a -> [Tree a]
children Empty = []
children (Node val trees) = trees

instance Arbitrary a => Arbitrary (Tree a) where arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = do
    a <- arbitrary
    return $ Node a []
arbTree n = do
    (Positive m) <- arbitrary
    let n' = n `div` (m + 1)
    f <- replicateM m (arbTree n')
    a <- arbitrary
    return $ Node a f

childrenBalancedImplication :: Tree a -> Bool
childrenBalancedImplication tree@(Node val children) = isBalanced tree <= all isBalanced children

childrenCountImplication :: Tree a -> Bool
childrenCountImplication Empty = True
childrenCountImplication (Node val []) = True
childrenCountImplication tree@(Node val children@(child1:otherChildren)) = isBalanced tree <= (foldl1 max childrenCounts * 10 <= (foldl1 min childrenCounts) * 25)
    where
        childrenCounts = map count children
        count :: Tree a -> Int
        count Empty = 0
        count tree@(Node val children) = 1 + (sum . map count $ children)

-- main = quickCheck implication
