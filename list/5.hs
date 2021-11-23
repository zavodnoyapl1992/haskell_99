myReverse :: [a] -> [a]

myReverse [] = []
myReverse (a:ax) = myReverse ax ++ [a]
