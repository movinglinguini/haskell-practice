-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = foldl (\acc cur -> acc ++ [(fib cur)]) [] [0..]

-- Exercise 2
fibs2' last next = last : fibs2' next (last + next)

fibs2 :: [Integer]
fibs2 = fibs2' 0 1

-- Exercise 3
data Stream a = Cons a (Stream a)
streamToList :: Stream a -> [a]
streamToList (Cons el s) = [el] ++ streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))
