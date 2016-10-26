{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches _        []       = 0
exactMatches []       _        = 0
exactMatches (x : xs) (y : ys) = (if x == y then 1 else 0) + exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = countIter xs colors
                 where countIter _  []       = []
                       countIter ps (c : ls) = [count c ps] ++ countIter ps ls
                       count x = length . filter (== x)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum . map (uncurry min) $ zip (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code

getMove :: Code -> Code -> Move
getMove actual guess = Move guess exactCount (totalCount - exactCount)
                       where exactCount = exactMatches actual guess
                             totalCount = matches actual guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess x y) actual = (x, y) == (exactCount , (totalCount - exactCount))
                                       where totalCount = matches guess actual
                                             exactCount = exactMatches guess actual

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (:[]) colors
allCodes n = concatMap (\x -> [x ++ [c] | c <- colors]) (allCodes (n-1))

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = go (allCodes (length secret)) []
               where go [] acc = reverse acc
                     go (g : guesses) acc = go (filterCodes move guesses) (move : acc)
                                            where move = getMove secret g

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined