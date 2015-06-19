import Data.Char (chr, ord, digitToInt)
import Data.List.Split (splitOn)
import Data.List (intersperse)

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