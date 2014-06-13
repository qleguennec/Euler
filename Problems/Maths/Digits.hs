module Problems.Maths.Digits (
    decDigs
    , isPalindromic
    , palindromes
    , digNumbers
    , nDigits
) where

import Data.Digits

decDigs :: Integer -> [Integer]
decDigs x = digits 10 x

isPalindromic :: Integer -> Bool
isPalindromic x = decDigs x == revDig
 where revDig = reverse $ decDigs x

palindromes :: [Integer]
palindromes = filter isPalindromic [1..]

digNumbers :: Integer -> Integer
digNumbers x = toInteger . length $ decDigs x

nDigits :: Integer -> (Integer, Integer) -> Bool
nDigits n (x, y) = and [(==n) $ digNumbers x, (==n) $ digNumbers y]
