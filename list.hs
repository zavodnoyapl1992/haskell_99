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
myReverse (a:ax) = a:(myReverse ax)


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


decodeModified :: Eq a => [CountNum a] -> [a]

decodeModified [] = []
decodeModified ((Single a):xs)= a:(decodeModified xs)
decodeModified ((Multiple 1 a):xs)= a:(decodeModified xs)
decodeModified ((Multiple len a):xs) = a:(decodeModified ((Multiple (len - 1) a):xs))


encodeDirect :: Eq a => [a] -> [CountNum a]

encodeDirect [] = []
encodeDirect (x:xs) = encodeD xs x 1 where
   encodeD [] prev count = [toType prev count]
   encodeD (x:xs) prev count | x == prev = encodeD xs prev (count + 1)
                             | otherwise = (toType prev count) : encodeD xs x 1
   toType a 1 = Single a
   toType a num = Multiple num a



dupli :: Eq a => [a] -> [a]

dupli [] = []
dupli (x:xs) = x:x:dupli xs


repli :: Eq a => [a] -> Integer -> [a]

repli [] _ = []
repli xs c = repli1 xs c c where
   repli1 [] _ _ = []
   repli1 (x:xs) 0 n2 = repli1 xs n2 n2
   repli1 xs@(x:_) n1 n2 = x:(repli1 xs (n1 - 1) n2)


dropEvery :: [a] -> Integer -> [a]

dropEvery xs n1 = dropEvery1 xs n1 n1 where
    dropEvery1 [] _ _ = []
    dropEvery1 (x:xs) i 1 = dropEvery1 xs i i
    dropEvery1 (x:xs) i i1 =  x:(dropEvery1 xs i (i1 - 1))


split :: [a] -> Integer -> ([a], [a])

split xs n1 = split1 xs n1 []
split1 xs 0 fs = (reverse fs, xs)
split1 (x:xs) c1 fs = split1 xs (c1 - 1) (x:fs)


slice :: [a] -> Integer -> Integer -> [a]

slice _ _ 0 = []
slice xs 0 end = slice xs 1 end
slice (x:xs) 1 1 = [x]
slice (x:xs) 1 en = x:(slice xs 1 (en-1))
slice (x:xs) st en | st > en = error "End less than start"
                   | otherwise = slice xs (st - 1) (en - 1)

rotate :: [a] -> Int -> [a]

rotate xs1@(x:xs) 0 = xs1
rotate xs1@(x:xs) n | n < 0 = rotate xs1 (length xs1 + n)
                    | otherwise = rotate (xs ++ [x]) (n - 1)


removeAt :: Integer -> [a] -> (a, [a])

removeAt = removeAt1 [] where
    removeAt1 _ 0 xs = error "empty remove"
    removeAt1 res 1 (x:xs) = (x, res ++ xs)
    removeAt1 _ n [] = error "empty remove"
    removeAt1 res n (x:xs) = removeAt1 (x:res) (n - 1) xs


insertAt :: a -> [a] -> Int -> [a]

insertAt a [] n = [a]
insertAt a xs 1 = a:xs
insertAt a (x:xs) n = x:(insertAt a xs (n -1))
