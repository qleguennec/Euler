module Problems.Maths.Divs (
 Div(..)
 , isDiv
 , divsOf
 , squareRoot
 , sdivsOf
 , sdivsMerged
 , divList
 , allDivs

 , multiplesOf
 , multiplesOfBelow
 , multSquareBelow

 , primes
 , isPrime
 , primesBelow
 , primesOf

 , square
) where

import Data.List
data Div = Div {number :: Integer, divs :: [Integer]}
 deriving (Eq, Ord, Show)

isDiv :: Integer -> Integer -> Bool
isDiv x n = (==0) $ mod x n

divsOf :: Integer -> Div
divsOf x = Div x [n | n <- [1..x], isDiv x n]

squareRoot :: Integer -> Integer
squareRoot = ceiling . sqrt . (fromIntegral :: Integer -> Double)

sdivsOf :: Integer -> Div
sdivsOf x = Div x $ sort . nub . filter (isDiv x) $ sqrtDivs ++ otherDivs
 where sqrtDivs  = filter (isDiv x) $ [1.. squareRoot x]
       otherDivs = map (div x) $ [1.. squareRoot x]

sdivsMerged :: Integer -> [(Integer, Integer)]
sdivsMerged x = sort . nub . map (\a -> (a, div x a)) . filter (isDiv x) $ [1.. squareRoot x]
 where sqrtDivs  = filter (isDiv x) $ [1.. squareRoot x]

divList :: [Integer] -> [Div]
divList xs = map sdivsOf xs

allDivs :: [Div]
allDivs = divList [1..]

multiplesOf :: Integer -> [Integer]
multiplesOf x = map (*x) [1..]

multSquareBelow :: Integer -> Integer -> [Integer]
multSquareBelow x below = [x*x, x*x+x.. below]

multiplesOfBelow :: Integer -> Integer -> [Integer]
multiplesOfBelow x below = takeWhile (<below) $ multiplesOf x

isPrime :: Integer -> Bool
isPrime x = (==2) $ length . divs $ sdivsOf x

primes :: [Integer]
primes = [x | x <- [1..], isPrime x]

primesOf :: Integer -> Div
primesOf x = Div x $ filter isPrime $ divs $ sdivsOf x

primesBelow :: Integer -> [Integer]
primesBelow below = sort $ rec_helper [2..below] []
    where
    rec_helper (x:xs) primes = 
        if (x<=squareRoot below) 
        then rec_helper (xs \\ (multSquareBelow x below)) (x : primes)
        else xs ++ primes
    rec_helper [] primes = primes

square :: Integer -> Integer
square x = x*x
