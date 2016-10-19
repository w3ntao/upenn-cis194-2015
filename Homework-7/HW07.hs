{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad.Random
import Data.Monoid
import Data.Vector (Vector, (!), (!?), (//))

import qualified Data.Vector as V

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mX = [ f x | x <- mX ]

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV x y vec = [ vec // [(y, vX), (x, vY)] | vX <- vec !? x, vY <- vec !? y ]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indexes vec = mapM (vec !?) indexes

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec = [ vec !? n | n <- getRandom ]

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = [ V.replicate n x | x <- getRandom ]

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (low, high) = [ V.replicate n x |  x <- getRandomR (low, high) ]

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = sfIter vec (length vec - 1)
              where sfIter xs 0 = return xs
                    sfIter xs i = do randJ <- getRandomR (0, i)
                                     sfIter (xs // [(i, xs ! randJ), (randJ, xs ! i)]) (i - 1)

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec n = (ltVec, pivot, gtVec)
                    where (ltVec, gtVec) = V.partition (< pivot) noPivot
                          noPivot = V.fromList [vec ! i | i <- [0..length vec - 1], i /= n]
                          pivot   = vec ! n

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort []       = []
quicksort (x : xs) =
    quicksort [ y | y <- xs, y < x ] <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec = if null vec
                then V.empty
                else qsort ltVec <> V.singleton pivot <> qsort gtVec
            where (ltVec, pivot, gtVec) = partitionAt vec 0

-- Exercise 8 -----------------------------------------

partitionR :: Ord a => Vector a -> Rnd (Vector a, a, Vector a)
partitionR vec = [ partitionAt vec randN | randN <- getRandomR (0, length vec - 1) ]

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec = if null vec
                 then return V.empty
                 else [ qsort ltVec <> V.singleton pivot <> qsort gtVec
                          | (ltVec, pivot, gtVec) <- partitionR vec ]

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i vec = if i < 0 || i > (length vec - 1)
                   then return Nothing
                   else do (ltVec, pivot, gtVec) <- partitionR vec
                           case compare i (length ltVec) of
                               LT -> select i ltVec
                               GT -> select (i - (length ltVec) - 1) gtVec
                               EQ -> return (Just pivot)

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = V.fromList [ Card label suit
               | label <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace],
                 suit  <- [Spade, Heart, Club, Diamond] ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck = if null deck
                    then Nothing
                    else Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck = if n == 0
                      then return ([], deck)
                      else do
                          (c,  deck0)  <- nextCard deck
                          (cs, deck1)  <- getCards (n-1) deck0
                          return (c : cs, deck1)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100