-- Exercise 1
getevery :: [a] -> Int -> Int -> [a]
getevery [] _ _ = []
getevery (a:as) n c
  | c > n = []
  | c < n = getevery as n (c + 1)
  | c == n = a : getevery as n 0

skips :: [a] -> [[a]]
skips as = skips_ as as 0

skips_ :: [a] -> [a] -> Int -> [[a]]
skips_ [] _ _ = []
skips_ (a:as1) as2 s = getevery as2 s 0 : skips_ as1 as2 (s + 1)

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima es
  | length es < 3 = []
localMaxima (e1:e2:e3:es)
  | e1 < e2 && e2 > e3 = e2 : localMaxima (e3:es)
  | otherwise = localMaxima (e2:e3:es)
