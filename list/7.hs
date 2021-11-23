data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]

flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (a:ax)) = flatten a ++ flatten (List ax)


