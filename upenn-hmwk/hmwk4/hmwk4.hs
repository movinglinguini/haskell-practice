-- Exercise 1: Wholemeal Programming

-- 1.
fun1 :: [Int] -> Int
fun1 = product . map (\x -> x - 2) . filter even


-- 2
fun2 :: Int -> Int
fun2 = sum . filter even . takeWhile (>1) . iterate(\n -> if (even n) then div n 2 else 3 * n + 1)

-- Exercise 2 : Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr makeTree Leaf

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

makeTree :: a -> Tree a -> Tree a
makeTree a Leaf = Node 0 Leaf a Leaf
makeTree a (Node h l v r)
  | height l <= height r = Node (height (makeTree a l) + 1) (makeTree a l) v r
  | otherwise = Node (height (makeTree a r) + 1) l v (makeTree a r)


-- Exercise 3 : More folds!

-- 1.
xor :: [Bool] -> Bool
xor = foldr (\nxt acc -> if nxt then not acc else acc) False

-- 2.
map' :: (a -> b) -> [a] -> [b]
map' fn = foldr (\nxt acc ->  fn nxt : acc) []

-- Exercise 4 : 
elementOf :: Integer -> [Integer] -> Bool
elementOf _ [] = False
elementOf n (h:hs) = (n == h) || elementOf n hs

computeExclusionList :: Integer -> [Integer]
computeExclusionList n = filter (<= n) [ i + j + 2 * i * j | i <- [1..n], j <- [1..n]]

sieve :: Integer -> [Integer]
sieve = map (\n -> 2 * n + 1) . filter (\n -> not (elementOf n (computeExclusionList n))) . takeWhile (>0) . iterate (\n -> n - 1)