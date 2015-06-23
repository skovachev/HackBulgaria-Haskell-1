-- ### 01. Map as foldl
-- Implement the function `map` using `foldl`

map' :: (a -> b) -> [a] -> [b]
map' func xs = foldl (\acc current -> acc ++ [func current]) [] xs

-- ### 02. Filter as foldl
-- Implement Prelude's `filter` using `foldl`
filter' :: (a -> Bool) -> [a] -> [a]
filter' func xs = foldl (accum_func) [] xs
    where accum_func acc current | func current = acc ++ [current]
                                 | otherwise = acc

-- ### 03. [Quicksort!](https://en.wikipedia.org/?title=Quicksort)
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort list = quicksort left_list ++ pivot_matches ++ quicksort right_list
    where list_length = length(list)
          pivot_index = list_length `div` 2
          pivot_value = list !! pivot_index
          left_list = filter (\x -> x < pivot_value) list
          right_list = filter (\x -> x > pivot_value) list
          pivot_matches = filter (\x -> x == pivot_value) list

-- ### 04. Repeat
-- Make an infinite list by repeating a given element
-- ```
-- repeat 1 -> [1, 1, 1, 1, 1, 1, 1..]
-- ```

repeat' :: a -> [a]
repeat' element = [element] ++ repeat' element

-- ### 05. Cycling!
-- Make an infinite list by repeating a given list
-- ```
-- cycle [1, 2] -> [1, 2, 1, 2, 1, 2..]
-- cycle [1, 2, 3] -> [1, 2, 3, 1, 2, 3, 1, 2, 3..]
-- cycle [] -> []
-- ```

cycle' :: [a] -> [a]
cycle' list = list ++ cycle' list

-- ### 06. Take every nth element from a list
-- ```
-- every 1 [1, 2, 3] -> [1, 2, 3]
-- every 2 [1, 2, 3] -> [2]
-- ```

every :: Int -> [a] -> [a]
every 0 (x:xs) = [x]
every position list = helper position 1 list
    where helper needed pos list | pos > length list = []
                                 | pos `mod` needed == 0 = (list !! (pos-1)) : helper needed (pos+1) list
                                 | otherwise = helper needed (pos+1) list

-- ### 07. Get the local maximas in a list of `Integer`s
-- ```
-- localMaxima [1..10] -> []
-- localMaxima [1, 2, 1, 2, 3, 0, 6] -> [2, 3]
-- ```

localMaxima :: Ord a => [a] -> [a]
localMaxima (x:y:z:xs) | x < y && y > z = y : localMaxima (y:z:xs)
                       | otherwise = localMaxima (y:z:xs)
localMaxima (_:_:[]) = []

-- ### 08. Map a function to a list of lists
-- ```
-- mapMap (^2) [[1], [1, 2, 3], []] -> [[1], [1, 4, 9], []]
-- ```

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap func list = map mapItem list
    where mapItem list = map (\x -> func x) list

-- ### 09. Filter a list of lists
-- ```
-- filterFilter ((== 0) . (`mod` 2)) [[1], [1, 2, 3], []] -> [[], [2], []]
-- ```

filterFilter :: (a -> Bool) -> [[a]] -> [[a]]
filterFilter func list = map filterItem list
    where filterItem list = filter func list

-- ### 10. Generate the unit matrix by given element and dimensions
-- ```
-- unit 1 3 -> [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
-- unit 2 2 -> [[2, 0], [0, 2]]
-- ```
unit :: a -> a -> [[b]]
unit element dimensions = undefined
