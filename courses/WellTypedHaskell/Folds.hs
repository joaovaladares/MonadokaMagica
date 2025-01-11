{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Folds where

import Prelude hiding (elem, map, and, or, filter, foldr, reverse, sum, length, foldl', flip)
import GHC.Base (minInt)

--------------------------------------------------------------------------------
-- foldr
--------------------------------------------------------------------------------
foldr :: (a -> r -> r) -> r -> [a] -> r
foldr cons nil []       = nil
foldr cons nil (x : xs) = cons x (foldr cons nil xs)

-- another way to write foldr
foldr' :: (a -> r -> r) -> r -> [a] -> r
foldr' cons nil = go
  where
    go []       = nil
    go (x : xs) = cons x (go xs)

elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem y (x : xs) = y == x || elem y xs

elem' :: Eq a => a -> [a] -> Bool
elem' y = foldr (\ x r -> x == y || r) False

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x r -> f x : r ) []

and :: [Bool] -> Bool
and []       = True
and (x : xs) = x && and xs

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\ x r -> if f x then x : r else r) []

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\ x r -> f x || r) False

length' :: [a] -> Int
length' = foldr (\ _ r -> 1 + r) 0

--------------------------------------------------------------------------------
-- foldl' is the same as foldl but strict
--------------------------------------------------------------------------------
foldl' :: (r -> a -> r) -> r -> [a] -> r
foldl' upd ini = go ini
  where
    go !acc [] = acc
    go !acc (x : xs) = go (upd acc x) xs

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

reverse :: [a] -> [a]
reverse = go []
  where
    go !acc []       = acc
    go !acc (x : xs) = go (x : acc) xs

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

sum :: Num a => [a] -> a
sum = go 0
  where
    go !acc []       = acc
    go !acc (x : xs) = go (acc + x) xs

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

length :: [a] -> Int
length = go 0
  where
    go !acc []       = acc
    go !acc (_ : xs) = go (acc + 1) xs

length'' :: [a] -> Int
length'' = foldl' (\ acc _ -> acc + 1) 0

maximum' :: (Ord a) => [a] -> Maybe a
maximum' []       = Nothing
maximum' (x : xs) = Just (foldl' max x xs)
