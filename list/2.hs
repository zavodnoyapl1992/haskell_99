myButLast :: [a] -> a

myButLast [] =  error "Empty list"
myButLast (_:[]) = error "Too few elements"
myButLast (a:_:[]) = a
myButLast (_:ax) = myButLast ax
