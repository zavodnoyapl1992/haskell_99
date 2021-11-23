myButLast :: [a] -> a

myButLast [] =  error "Empty list"
myButLast (a:[]) = error "Too few elements"
myButLast (a:_:[]) = a
myButLast (a:ax) = myButLast ax
