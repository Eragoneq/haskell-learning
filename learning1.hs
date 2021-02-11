-- Exercise 1

toDigits :: Integer -> [Integer]
toRevDigits :: Integer -> [Integer]

toDigits num
    | num <= 0 = []
    | num < 10 = [num]
    | otherwise = toDigits (num `div` 10) ++ [num `mod` 10]

-- This could just be the upper algorithm 
toRevDigits num
    | num <= 0 = []
    | num < 10 = [num]
    | otherwise = (num `mod` 10) : toRevDigits (num `div` 10)

-- Exercise 2

-- Kinda weird solution, but it could be improved after I learn a bit more
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs)
    | even (length (x:y:xs)) = (2*x) : y : doubleEveryOther xs
    | otherwise = x : (2*y) : doubleEveryOther xs
doubleEveryOther (x:xs) = [x]
doubleEveryOther _ = []

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits list = sum (map (sum . toDigits) list)

-- Exercise 4
-- This chain of functions can probably be done in a better way, but I didn't read enough yet

validate :: Integer -> Bool
validate num = sumDigits (doubleEveryOther (toDigits num)) `mod` 10 == 0