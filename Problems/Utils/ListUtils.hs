module Problems.Utils.ListUtils (
    listSequences
    , strToList
    , remove
    , addLine
) where

import Data.Digits

listSequences :: Int -> [a] -> [[a]]
listSequences length (x:xs) = [(take length $ x : xs)] ++ listSequences length xs
listSequences _ [] = []

strToList :: String -> [Int]
strToList str = digits 10 (read str :: Int)

remove :: (Eq a) => a -> [a] -> [a]
remove c str = [x | x <- str, x /= c]

addLine :: String -> String
addLine str = '\n' : str
