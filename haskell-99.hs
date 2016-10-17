import Data.List as L
--p1
myLast =  head . reverse

--p2
myButLast = head . drop 1 . reverse

--p3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt xs n
  | n < 1 = error "Index out of bounds"
  | otherwise = last . take n $ xs

--p4
myLength :: [x] -> Int
myLength = foldl (\ acc _ -> acc + 1) 0

--p5
myReverse :: [a] -> [a]
myReverse = foldl (flip(:)) []

myReverse' [] = []
myReverse' (x:[]) = [x]
myReverse' (x:xs) = myReverse' xs ++ [x]

--p6
isPalindrome xs = xs == reverse xs

isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' (x:xs) = (x == last xs) && (isPalindrome' $ init xs)

--p7
data NestedList a = Elem a | List [NestedList a]

--p8
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs then compress xs else x:(compress xs)

compress' :: (Eq a) => [a] -> [a]
compress' =  map head . L.group
