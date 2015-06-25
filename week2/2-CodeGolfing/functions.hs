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
unit :: (Ord a, Num a, Num t) => t -> a -> [[t]]
unit element dimensions = generate_matrix 0
  where generate_matrix current_row | current_row >= dimensions = []
                                    | otherwise = [generate_row current_row]++generate_matrix (current_row+1)
        generate_row pos = generate_item pos 0
        generate_item pos current_pos | current_pos >= dimensions = []
                                      | current_pos == pos = [element]++generate_item pos (current_pos+1)
                                      | otherwise = [0]++generate_item pos (current_pos+1)


-- ### 11. Get the nth row and column of a matrix
-- ```
-- row 1 [[1, 2, 3], [2, 3, 4]] -> [2, 3, 4]
-- row 8 (unit 1 10) -> [0, 0, 0, 0, 0, 0, 0, 0, 1, 0]
-- ```
row :: Int -> [a] -> a
row n matrix = matrix !! n

-- ### 12. Transpose a matrix
-- ```
-- transpose' [[1, 2, 3], [2, 3, 4]] -> [[1, 2], [2, 3], [3, 4]]
-- transpose' (unit 1 2) -> [[1, 0], [0, 1]]
-- transpose' [[1, 2], [3, 4], [5, 6]] -> [[1, 3, 5], [2, 4, 6]]
-- ```
transpose' :: [[a]] -> [[a]]
transpose' matrix = [ (new_row y 0) | y <- [0..max_y_idx] ] 
  where max_y_idx = (length (matrix !! 0)) - 1
        num_rows = length matrix
        new_row item_pos current_row | current_row >= num_rows = []
                                     | otherwise = [(matrix !! current_row !! item_pos)] ++ new_row item_pos (current_row+1)

-- ### 13. Sum of matrices
-- ```
-- sumMatrices (unit 2 2) (unit 3 2) == unit 5 2 -> True
-- ```
sumMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrices matrix1 matrix2 | length matrix1 /= length matrix2 || length (matrix1 !! 0) /= length (matrix2 !! 0) = error "Cannot sum matrices"
                            | otherwise = [ calculate_item x | x <- [0..x_max] ]
  where x_max = length matrix1 - 1
        y_max = length (matrix1 !! 0) - 1
        calculate_item x = [ (matrix1 !! x !! y) + (matrix2 !! x !! y) | y <- [0..y_max] ]

-- ### 14. Multiply matrices
-- ```
-- multMatrices [[1, 2], [3, 4]] [[1, 2], [3, 4]] -> [[7, 10], [15, 22]]
-- ```
multMatrices :: Num t => [[t]] -> [[t]] -> [[t]]
multMatrices matrix1 matrix2 = [ calculate_row z | z <- [0..y_max] ]
  where x_max = length matrix1 - 1
        y_max = length (matrix1 !! 0) - 1
        matrix2_t = transpose' matrix2
        calculate_row z = [ calculate_item z y | y <- [0..y_max] ]
        calculate_item z y = sum [ (matrix1 !! x !! y) * (matrix2_t !! x !! z) | x <- [0..x_max] ]
        
-- ### 15. Histogram
-- ```
-- histogram [1, 2, 1, 3, 1] ->
-- " *
--   *
--   * * *
-- 0 1 2 3 4 5 6 7 8 9"
-- ```
