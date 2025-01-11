{-# OPTIONS_GHC -Wall #-}

module Composition where

import Prelude hiding ((.), id, and, ($), replicate)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

sumEvenSquares :: [Int] -> Int
sumEvenSquares xs = sum (map (\x -> x * x) (filter even xs))

sumEvenSquares' :: [Int] -> Int
sumEvenSquares' =  sum . map (\x -> x * x) . filter even

countLongerThanFive :: [[a]] -> Int
countLongerThanFive xs = length (filter (\x -> length x >= 5) xs)

countLongerThanFive' :: [[a]] -> Int
countLongerThanFive' = length . filter (\x -> length x >= 5)

countLongerThanFive'' :: [[a]] -> Int
countLongerThanFive'' = length. filter ((>= 5) . length)

id :: a -> a
id x = x

($) :: (a -> b) -> a -> b
f $ x = f x
 
and :: [Bool] -> Bool
and = all id

listOfFunctions :: [Int -> Int]
listOfFunctions = [(+1), (*2), (+7), (*4)]

replicate :: Int -> a -> [a]
replicate n x = take n $ repeat x
