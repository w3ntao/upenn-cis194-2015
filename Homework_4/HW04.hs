{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P []       == P []       = True
    P []       == _          = False
    _          == P []       = False
    P (w : ws) == P (z : zs) = w == z && P ws == P zs

    P w /= P z = not $ P w == P z

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P poly) = if all (==0) poly
                        then "0"
                        else insert " + " (breakP (P (reverse poly)))
                    where breakP (P [])       = []
                          breakP (P (y : ys)) = (showF y (length ys)) ++ breakP (P ys)
                          showF base n | base == 0           = []
                                       | base == 1 && n == 0 = ["1"]
                                       | otherwise           = [showBase base ++ showExp n]
                          showBase y | y == 1    = ""
                                     | y == -1   = "-"
                                     | otherwise = show y
                          showExp n | n == 0    = ""
                                    | n == 1    = "x"
                                    | otherwise = "x^" ++ show n
                          insert slice = (drop (length slice)) . concat . zipWith (++) (repeat slice)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P w) (P z) = P (zipWith (+) long (short ++ repeat 0))
                   where (short, long) = if length w > length z
                                             then (z, w)
                                             else (w, z)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P [])       (P _)  = P [0]
times (P (w : ws)) (P zs) = P (map (*w) zs) + times (P ws) (P ([0] ++ zs))

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P ys) = P (map (* (-1)) ys)
    fromInteger y = P [fromInteger y]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P [])       _ = 0
applyP (P (y : ys)) w = y + w * (if null ys then 0 else applyP (P ys) w)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f = (iterate deriv f) !! n

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P [])       = P [0]
    deriv (P (_ : ys)) = P (zipWith (*) ys (map fromInteger [1..]))