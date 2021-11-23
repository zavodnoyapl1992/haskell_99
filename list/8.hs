compress :: Eq a => [a] -> [a]

compress [] = []
compress ax = foldr ch [] ax where
       ch x [] = [x]
       ch x xs@(t:_) | t == x = xs
                     | otherwise = x:xs

