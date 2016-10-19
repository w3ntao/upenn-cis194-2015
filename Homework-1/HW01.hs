{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits x = [lastDigit x] ++ toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = doubleSkip True
                   where doubleSkip switch (x: xs) =
                             (if switch then x else x*2): doubleSkip (not switch) xs
                         doubleSkip _      []      = []

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . (map digit)
            where digit x = lastDigit x + (if x < 10 then 0 else (digit . dropLastDigit) x)

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn number = (sumDigits . doubleEveryOther . toRevDigits) number `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 1 start dest _    = [(start, dest)]
hanoi3 n start dest temp = hanoi3 (n-1) start temp dest ++ [(start, dest)] ++ hanoi3 (n-1) temp dest start

-- Exercise 7 -----------------------------------------

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 start dest _     _     = hanoi3 1 start dest ""
hanoi4 n start dest tempX tempY = moveTopK ++ moveBottom ++ moveK
                                  where k = n - ((round . (sqrt :: Double -> Double) . fromIntegral) (2*n + 1)) + 1
                                        moveTopK   = hanoi4 k start tempX dest tempY
                                        moveBottom = hanoi3 (n-k) start dest tempY
                                        moveK      = hanoi4 k tempX dest start tempY