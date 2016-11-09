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

--p9
pack [] = []
pack [x] = [[x]]
pack (x:xs)
  | x == head currentPack = (x:currentPack) : (tail packed)
  | otherwise = [x]:packed
  where packed = pack xs
        currentPack = head packed

pack' (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack' rest
pack' [] = []

pack'' [] = []
pack'' xx@(x:xs) = (takeWhile (==x) xx) : (pack'' $ dropWhile (==x) xx)

--p11
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

--p12
data Encoded a = Multiple Int a | Single a deriving (Show, Eq)

encode' :: Eq a => [a] -> [Encoded a]
encode' = map(\x -> encodePack x) . pack
  where encodePack (x:[]) = Single x
        encodePack xx@(x:xs) = Multiple (length xx) x

--p13

addToEncode :: Eq a => a -> [Encoded a] -> [Encoded a]
addToEncode a [] = [Single a]
addToEncode a (x@(Single b):xs) = if a == b
                                    then (Multiple 2 a):xs
                                    else (Single a) : x : xs
addToEncode a (x@(Multiple n b) : xs) = if a == b
                                          then (Multiple (n + 1) b) : xs
                                          else (Single a) : x : xs

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = foldr addToEncode []

--p14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--p15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = ((take n) . repeat $ x) ++ (repli xs n)

--p16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs) ++ (dropEvery (drop n xs) n)

--p17
split :: [a] -> Int -> ([a], [a])
-- split [] _ = ([], [])
-- split xs 0 = ([], xs)
-- split xs n = (take n xs, drop n xs)

split [] _ = ([], [])
split xs 0 = ([], xs)
split (x: xs) n = let (a, b) = split xs (n - 1)
                  in (x:a, b)

-- split xs n = foldl (\acc@(first, second) x -> if (length first) == n
--                                 then (first, second ++ [x])
--                                 else (first ++ [x], second)) ([], []) xs

--p18
slice :: [a] -> Int -> Int -> [a]
slice xs start end = fst (split (snd (split xs (start - 1))) (end - start + 1))
