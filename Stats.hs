e :: Fractional a => [a] -> [a] -> a
e ps = sum . zipWith (*) ps

cumul :: Num a => [a] -> [a]
cumul = scanl (+) 0
