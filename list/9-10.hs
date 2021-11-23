pack :: Eq a => [a] -> [[a]]

pack [] = []
pack ax = foldr ch [] ax where
       ch x [] = [[x]]
       ch x xs1@(t:xs) | head t == x = (x:t):xs
                       | otherwise = [x]:xs1


encode :: Eq a => [a] -> [(Int, a)]


encode = map (\x -> (length x, head x)) . pack
