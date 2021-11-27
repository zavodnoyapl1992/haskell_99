myLast :: [a] -> a

myLast [] =  error "Empty list"
myLast (a:[]) = a
myLast (a:ax) = myLast ax


myButLast :: [a] -> a

myButLast [] =  error "Empty list"
myButLast (_:[]) = error "Too few elements"
myButLast (a:_:[]) = a
myButLast (_:ax) = myButLast ax

elementAt :: [a] -> Integer -> a

elementAt [] _ = error "Not found element"
elementAt (a:_) 1 = a
elementAt (a:ax) x | x > 1 = elementAt ax (x - 1)
                   | otherwise = error "Not found element"

myLength :: [a] -> Integer

myLength [] = 0
myLength (_:[]) = 1
myLength (_:ax) = 1 + myLength ax


myReverse :: [a] -> [a]

myReverse [] = []
myReverse (a:ax) = myReverse ax ++ [a]


isPalindrome :: Eq a => [a] -> Bool

isPalindrome ax = ax == reverse ax


data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]

flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (a:ax)) = flatten a ++ flatten (List ax)


compress :: Eq a => [a] -> [a]

compress [] = []
compress ax = foldr ch [] ax where
       ch x [] = [x]
       ch x xs@(t:_) | t == x = xs
                     | otherwise = x:xs

pack :: Eq a => [a] -> [[a]]

pack [] = []
pack ax = foldr ch [] ax where
       ch x [] = [[x]]
       ch x xs1@(t:xs) | head t == x = (x:t):xs
                       | otherwise = [x]:xs1


encode :: Eq a => [a] -> [(Int, a)]

encode = map (\x -> (length x, head x)) . pack


data CountNum a = Multiple Int a | Single a deriving (Show)

encodeModified :: Eq a => [a] -> [CountNum a]

encodeModified = map toType . pack where
   toType [x] = Single x
   toType xs@(x:_) = Multiple (length xs) x
