{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = [1, 1] ++ zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) =  Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) ys = Cons x (sInterleave ys xs)

sTake :: Int -> Stream a -> [a]
sTake 0 _           = []
sTake n (Cons x xs) = x : sTake (n-1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) (fmap (+1) ruler)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand n = randSeq n
         where randSeq x = Cons x (randSeq ((1103515245 * x + 12345) `mod` 2147483648))

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 235 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax []       = Nothing
minMax (n : ns) = minMaxIter ns n n
    where minMaxIter []       tMin tMax = Just (tMin, tMax)
          minMaxIter (x : xs) tMin tMax | x < tMin  = minMaxIter xs x    tMax
                                        | x > tMax  = minMaxIter xs tMin x
                                        | otherwise = minMaxIter xs tMin tMax

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix a = Matrix a a a a

instance Num a => Num (Matrix a) where
    (Matrix x00 x01 x10 x11) * (Matrix y00 y01 y10 y11) =
        Matrix (seqMul x00 x01 y00 y10)
               (seqMul x00 x01 y01 y11)
               (seqMul x10 x11 y00 y10)
               (seqMul x10 x11 y10 y11)
        where seqMul w z s t = w*s + z*t

    (+) = undefined
    abs = undefined
    signum = undefined
    negate = undefined
    fromInteger = undefined

fastFib :: Int -> Integer
fastFib n = fetch $ fastExp (Matrix 1 1 1 0) n
            where fetch (Matrix _ x _ _) = x
                  fastExp x y            = fExp x y x
                  fExp _ 0 z = z
                  fExp x y z = if even y
                                   then fExp (x*x) (y `div` 2) z
                                   else fExp x     (y - 1)     (x*z)