myLast =  head . reverse
myButLast = head . drop 1 . reverse

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt xs n
  | n < 1 = error "Index out of bounds"
  | otherwise = last . take n $ xs
