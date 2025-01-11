{-# OPTIONS_GHC -Wall #-}

module Functions where

import           Prelude hiding (any, drop, filter, map, (++))

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

any :: (a -> Bool) -> [a] -> Bool
any _ []       = False
any f (x : xs) = f x || any f xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n (x : xs)
  | n <= 0 = x : xs
  | otherwise = drop (n - 1) xs

append :: [a] -> [a] -> [a]
append [] ys       = ys
append (x : xs) ys = x : append xs ys

(++) :: [a] -> [a] -> [a]
[] ++ ys       = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- If the list xs does not have sufficient elements or i and l are negative,
-- it will return an empty list
slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice i l xs = take l (drop i xs)

snoc :: [a] -> a -> [a]
snoc xs n = xs ++ [n]

snoc' :: [a] -> a -> [a]
snoc' [] n       = [n]
snoc' (x : xs) n = x : snoc' xs n

nub :: (Eq a) => [a] -> [a]
nub []       = []
nub (x : xs) = x : filter (x /=) (nub xs)
