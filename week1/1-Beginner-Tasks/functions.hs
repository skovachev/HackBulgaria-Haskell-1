import Prelude hiding (splitAt)
import Data.Char (chr, ord, digitToInt)
import Data.List (splitAt)

even' :: Integer -> Bool
even' number = number `mod` 2 == 0

odd' :: Integer -> Bool
odd' number = not (even' number)

bmi :: Double -> Double -> Double
bmi height weight = weight / height**2

deg2Rad, rad2deg :: Double -> Double
deg2Rad deg = deg * pi / 180

rad2deg rad = rad * 180 / pi

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a > 0 && b > 0 && c > 0 && (a + b) > c && (b + c) > a && (a + c) > b

perimeter :: [Double] -> Double
perimeter a = sum a

area :: Double -> Double -> Double -> Double
area a b c = sqrt (halfPerimeter * (halfPerimeter - a)  * (halfPerimeter - b) * (halfPerimeter - c))
    where p = perimeter [a, b, c] 
          halfPerimeter = p / 2

calculate :: Char -> Integer -> Integer -> Integer
calculate '+' a b = a + b
calculate '-' a b = a - b
calculate '*' a b = a * b
calculate _ a b = error "not supported"

convert :: String -> String -> Float -> Float
convert cur1 cur2 amount
   | cur1 == "usd" && cur2 == "bgn" = amount * 1.74
   | cur1 == "bgn" && cur2 == "usd" = amount / 1.74
   | cur1 == "eur" && cur2 == "bgn" = amount * 1.96
   | cur1 == "bgn" && cur2 == "eur" = amount / 1.96
   | cur1 == "usd" && cur2 == "eur" = amount * 1.12
   | cur1 == "eur" && cur2 == "usd" = amount / 1.12
   | otherwise = error "unsupported currencies"

head' :: [a] -> a
head' (x:_) = x
head' [] = error "Head called on empty list"

tail' :: [a] -> [a]
tail' (_:xs) = xs

-- 16. Last
-- last' [1, 2, 3] -> 3
-- last' [] -> an error message
last' :: [a] -> a
last' [] = error "Cannot run on empty list"
last' a = head' (reverse a)

-- last' using recursion
-- last' (x:[]) = x
-- last' (_:xs) = last' xs

-- 17. Double all elements
-- double [1, 2, 3] -> [2, 4, 6]
-- double [] -> []
double :: [Int] -> [Int]
double [] = []
double (x:xs) = x*2 : double xs

-- 18. More generic - make it possible to multiply all elements in a list with a given number
-- mult 3 [1, 2, 3] -> [3, 6, 9]
-- mult 42 [] -> []
mult :: Integer -> [Integer] -> [Integer]
mult multiplier [] = []
mult multiplier (x:xs) = x*multiplier : mult multiplier xs

-- 19. Get the n-th element of a list
-- nth 3 [1, 2, 3, 4] -> 4
-- nth 0 [1, 2, 3] -> 1
-- nth 2 [1] -> error message
nth :: Int -> [a] -> a
nth 0 list = head' list
nth pos (x:xs) | pos < 0 = error "Invalid index"
               | otherwise = nth (pos-1) xs

-- 20. Is an element member of a list?
-- member 3 [1, 2, 3] -> True
-- member 2 [0, 1, 3] -> False
member :: Int -> [Int] -> Bool
member _ [] = False
member value list = if value == current_value 
                    then True 
                    else member value (tail' list)
    where current_value = nth 0 list

-- 21. Is the list a fibonacci sequence?
-- isFibonacciSequence [1, 1] -> True
-- isFibonacciSequence [0, 1, 1, 2, 3, 5, 8, 13] -> True
-- isFibonacciSequence [1, 1, 1, 2] -> False
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

isFibonacciSequence :: [Int] -> Bool
isFibonacciSequence list = (last' list) == fib((length list) - 1)


-- 22. Get the sum of a list
-- sum' [1, 2, 10] -> 13
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

-- 23. Get the product of a list
-- product' [1, 2, 10] -> 20
product' :: [Int] -> Int
product' [] = 0
product' (x:xs) = x * (product' xs)

-- 24. Multiply lists
-- mult [1, 2, 3] [2, 3, 4] -> [2, 6, 18]
-- mult [3] [1, 2, 3] -> [3]
mult_lists :: [Int] -> [Int] -> [Int]
mult_lists [] _ = []
mult_lists (x:xs) (y:ys) = x*y:(mult_lists xs ys)

-- 25. Number to string
-- number2string 123 -> "123"
-- Hint: import Data.Char (chr, ord)
number2string :: Integer -> String
number2string number = show number

-- 26. String to number
-- string2number "3212" -> 3212
string2number :: String -> Integer
string2number string = read string

-- 27. Is valid ID?
-- isValidID "6101047500" -> True

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

apply_weight :: Char -> Int -> Int
apply_weight numberChar pos = digitToInt(numberChar) * (weights !! pos)
  where weights = [2, 4, 8, 5, 10, 9, 7, 3, 6, 0] -- last weight is 0 to skip it in the sum

calculate_last_id_number :: String -> Int
calculate_last_id_number idNumber | result < 10 = result
                                  | otherwise = 0
  where result = sum(mapInd apply_weight idNumber) `div` 11

isValidID :: String -> Bool
isValidID idNumber | length(idNumber) /= 10 = False
                   | otherwise = first_pair_ok && second_pair_ok && third_pair_ok && digitToInt(idNumber !! 9) == last_number
  where first_pair = read ([idNumber !! 0] ++ [idNumber !! 1])
        first_pair_ok = first_pair >= 0 && first_pair <= 99
        second_pair = read ([idNumber !! 2] ++ [idNumber !! 3])
        second_pair_ok = (second_pair > 0 && second_pair < 13) || (second_pair > 20 && second_pair < 33) || (second_pair > 40 && second_pair < 53)
        third_pair = read ([idNumber !! 4] ++ [idNumber !! 5])
        third_pair_ok = (third_pair > 0 && second_pair < 32) -- check if valid day for month X
        last_number = calculate_last_id_number idNumber

-- 28. Get the zodiac sign from an ID
-- whatZodiacSignIs "6101047500" -> "Capricorn"
-- TODO: just the month is not enough to accurately determine zodiac sign
whatZodiacSignIs :: String -> String
whatZodiacSignIs idNumber | month == 4 = "Aries"
                          | month == 5 = "Taurus"
                          | month == 6 = "Gemini"
                          | month == 7 = "Cancer"
                          | month == 8 = "Leo"
                          | month == 9 = "Virgo"
                          | month == 10 = "Libra"
                          | month == 11 = "Scorpio"
                          | month == 12 = "Sagittarius"
                          | month == 1 = "Capricorn"
                          | month == 2 = "Aquarius"
                          | otherwise = "Pisces"
  where second_pair = read ([idNumber !! 2] ++ [idNumber !! 3])
        month = second_pair `mod` 20


-- 30. Concatenate the lists
-- concatenate [1, 2, 3] [2, 3, 4] -> [1, 2, 3, 2, 3, 4]
concatenate :: [a] -> [a] -> [a]
concatenate list1 list2 = list1 ++ list2

-- 31. Take all elements of a list without the last one
-- init' [1, 2, 3] -> [1, 2]
-- init' []        -> "You can't do that with the empty list!"
init' :: [a] -> [a]
init' [] = error "You can't do that with the empty list!"
init' list = fst (splitAt (length(list)-1) list)

-- 32. Take the first n elements from a list
-- take' 3 [1..10] -> [1, 2, 3]
-- take' 3 [1, 2]  -> [1, 2]

take' :: Int -> [a] -> [a]
take' count list | count > 0 && count <= list_length = head' list:(take' (count-1) (tail' list))
                 | count == 0 = []
                 | otherwise = take' (count-1) list
  where list_length = length list


-- 33. Drop the first n elements from a list
-- drop' 3 [1..10] -> [4, 5, 6, 7, 8, 9, 10]
-- drop' 3 [1, 2]  -> []

drop' :: Int -> [a] -> [a]
drop' 0 list = list
drop' count list = drop' (count-1) (tail' list)


-- 34. Zipping lists
-- zip' [1, 2, 3] ['a', 'b', 'c'] -> [(1, 'a'), (2, 'b'), (3, 'c')]
-- zip' ["we", "like", "to"] ["party"] -> [("we", "party")]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' list1 list2 = (head' list1, head' list2):(zip' (tail' list1) (tail' list2))

-- 35. Now unzip it!
-- unzip' [(1, 2), (2, 3), (3, 4)] -> ([1, 2, 3], [2, 3, 4])
-- unzip' [("I", "surely"), ("do", "not"), ("like", "you")] -> (["I", "do", "like"], ["surely", "not", "you"])

tupleCat (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' list = tupleCat ([elementA], [elementB]) (unzip' remaining_tuples)
  where tuple = head' list
        remaining_tuples = tail' list
        elementA = fst tuple
        elementB = snd tuple

-- 36. Grouping
-- group' [1, 1, 2, 2, 1, 1, 3] -> [[1, 1], [2, 2], [1, 1], [3]]
-- group' [1..5] -> [1, 2, 3, 4, 5]

group' :: Eq a => [a] -> [[a]]
group' (x:xs) = (x:matched_elements):group' remainder
  where (matched_elements, remainder) = span (== x) xs
group' [] = []

-- 37. Generate all pythagorean triples
-- pyths 1 10 -> [(3,4,5), (6,8,10)]

pyths :: a -> a -> [(a,a,a)]
pyths start end = undefined
-- TODO

-- 38. Return a function, which multiplies a number by a factor
-- let multiplyByTwo = multiplyBy 2
-- multiplyByTwo 3 -> 6
-- multiplyByTwo 9 -> 18

multiplyBy :: Num a => a -> a -> a
multiplyBy number = \y -> y * number

-- 39. Get the last digit of all numbers in a list
-- lastDigits [10..19] -> [0,1,2,3,4,5,6,7,8,9]
lastDigits :: Integral a => [a] -> [a]
lastDigits range = [ x `mod` 10 | x <- range ]
-- lastDigits = applyToAll (`mod` 10)

-- 40. Turn all strings in a list to integers
-- You can expect that the input will be always valid!
-- stringsToIntegers ["7", "42", "13"] -> [7, 42, 13]

stringsToIntegers :: [String] -> [Integer]
stringsToIntegers strings = [string2number x | x <- strings]
-- stringsToIntegers = applyToAll string2number

-- 41. Get the fibonacci numbers with the corresponding indices
-- fibonaccis [0..5] -> [0, 1, 1, 2, 3, 5]
-- fibonaccis [5, 10, 15] -> [5, 55, 610]

fibonaccis :: [Int] -> [Int]
fibonaccis numbers = [ fibonacci_list !! x | x <- numbers ]
    where fibonacci_up_to = last numbers
          fibonacci_list = 0:[ fib x | x <- [0..fibonacci_up_to]]


-- 42. Take a function and apply it to all elements of a list
-- let lastDigits = applyToAll (`mod` 10)
-- lastDigits [10..19] -> [0,1,2,3,4,5,6,7,8,9]
applyToAll :: (a -> a) -> [a] -> [a]
applyToAll func = \range -> [ func x | x <- range ]


-- 45. More generic - return a function that filters all the numbers in a list divisible by 'n'
-- let divisibleByThree = divisibles 3
-- divisibleByThree [1..10] -> [3,6,9]
divisibles :: Integral t1 => t -> [t1] -> [t1]
divisibles byNumber = \range -> [ x | x <- range, x `mod` 3 == 0]
-- divisibles byNumber = filterBy (\x -> x `mod` byNumber == 0)

-- 46. Take a predicate and filter a list
-- let odds' = filterBy odd
-- odds' [1..10] -> [1,3,5,7,9]
filterBy :: (a -> Bool) -> [a] -> [a]
filterBy func (x:xs) | func x = x : next_call
                     | otherwise = next_call
  where next_call = filterBy func xs
filterBy func _ = []

-- 48. Get the product of a list
-- product' [1..5] -> 120
product2 = foldl' (\a b -> a * b)

-- 49. Concatenate the lists
-- concat' [[1, 2, 3], [2, 3, 4], []] -> [1,2,3,2,3,4]
concat' :: [[a]] -> [a]
concat' (x:xs) = x ++ concat' xs
concat' _ = []

-- 50. Reducing!
-- let sum'' = reduce (+) 0
-- sum'' [1..10] -> 55
reduce :: (a -> a -> a) -> a -> a



map' :: (a -> b) -> [a] -> [b]
map' func = \range -> [ func x | x <- range]

filter' :: (a -> Bool) -> [a] -> [a]
filter' func = \range -> [ x | x <- range, func x ]

foldl' :: Num a => (a -> a -> a) -> [a] -> a
foldl' func [x] = x
foldl' func (x:xs) = func x (foldl' func xs)
