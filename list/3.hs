elementAt :: [a] -> Integer -> a

elementAt [] _ = error "Not found element"
elementAt (a:_) 1 = a
elementAt (a:ax) x | x > 1 = elementAt ax (x - 1)
                   | otherwise = error "Not found element"
