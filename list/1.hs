myLast :: [a] -> a

myLast [] =  error "Empty list"
myLast (a:[]) = a
myLast (a:ax) = myLast ax
