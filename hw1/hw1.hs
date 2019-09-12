{-# OPTIONS_GHC -Wall #-}

-- Credit Card Validation

-- Convert Integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- toDigits but reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- doubles every other integer
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : y * 2 : (doubleEveryOther xs)

-- sums the individual digits of every integer in the list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum . toDigits $ x) + sumDigits(xs)

-- performs credit card validation
-- 1. double every second digit from the right
-- 2. add digits doubled and undoubled values
-- 3. calculate remainder when sum is divided by 10 (0 means valid)
validate :: Integer -> Bool
validate num = (sumDigits . doubleEveryOther . toDigitsRev $ num) `mod` 10 == 0


-- Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 = (hanoi (n-1) p1 p3 p2) ++ [(p1, p2)] ++ (hanoi (n-1) p3 p2 p1)
