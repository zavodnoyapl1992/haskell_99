myLength :: [a] -> Integer

myLength [] = 0
myLength (_:[]) = 1
myLength (_:ax) = 1 + myLength ax
