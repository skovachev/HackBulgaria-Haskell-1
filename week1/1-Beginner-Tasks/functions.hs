import Data.Char (chr, ord)

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
isValidID :: String -> Bool


-- 28. Get the zodiac sign from an ID
-- whatZodiacSignIs "6101047500" -> "Capricorn"
whatZodiacSignIs :: String -> String