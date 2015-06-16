even' :: Integer -> Bool
even' number = if number `mod` 2 == 0 then True else False

odd' :: Integer -> Bool
odd' number = not (even' number)

bmi :: Double -> Double -> Double
bmi height weight = weight / height**2

deg2Rad :: Double -> Double
deg2Rad deg = deg * pi / 180

rad2deg :: Double -> Double
rad2deg rad = rad * 180 / pi

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = if a == b && b == c then True else False

perimeter :: Double -> Double -> Double -> Double
perimeter a b c = a + b + c

area :: Double -> Double -> Double -> Double
area a b c = sqrt ((perimeter a b c) / 2 * ((perimeter a b c) / 2 -a)  * ((perimeter a b c) / 2 -b) * ((perimeter a b c) / 2 - c))

calculate :: Char -> Integer -> Integer -> Integer
calculate operator a b = if operator == '+' then a + b else (if operator == '-' then a - b else (if operator == '*' then a * b else error "not supported"))

convert :: String -> String -> Float -> Float
convert cur1 cur2 amount
   | cur1 == "usd" && cur2 == "bgn" = amount * 1.74
   | cur1 == "bgn" && cur2 == "usd" = amount / 1.74
   | cur1 == "eur" && cur2 == "bgn" = amount * 1.96
   | cur1 == "bgn" && cur2 == "eur" = amount / 1.96
   | cur1 == "usd" && cur2 == "eur" = amount * 1.12
   | cur1 == "eur" && cur2 == "usd" = amount / 1.12
   | otherwise            = error "unsupported currencies"