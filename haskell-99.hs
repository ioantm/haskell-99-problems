myLast =  head . reverse
myButLast = head . drop 1 . reverse

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt xs n
  | n < 1 = error "Index out of bounds"
  | otherwise = last . take n $ xs

myLength :: [x] -> Int
myLength = foldl (\ acc _ -> acc + 1) 0

myReverse :: [a] -> [a]
myReverse = foldl (flip(:)) []

myReverse' [] = []
myReverse' (x:[]) = [x]
myReverse' (x:xs) = myReverse' xs ++ [x]
