toDigits :: Integer -> [Integer]
toRevDigits :: Integer -> [Integer]

toDigits num
    | num <= 0 = []
    | num < 10 = [num]
    | otherwise = toDigits (num `div` 10) ++ [num `mod` 10]

toRevDigits num
    | num <= 0 = []
    | num < 10 = [num]
    | otherwise = (num `mod` 10) : toRevDigits (num `div` 10)