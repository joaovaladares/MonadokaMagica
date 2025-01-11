--------------------------------------------------------------------------
-- A.hs
--
-- (c) 2012-2024 Andres Loeh, Well-Typed LLP
{-# OPTIONS_GHC -Wall #-}

-- (the above line enables more compiler warnings; useful for beginners)
module A where

--------------------------------------------------------------------------
-- Introduction:
--
-- In this module, the goal is to write some basic functions.
--
-- Many of these functions are actually available either in the Prelude
-- or from the standard libraries. However, the goal here is to
-- reimplement them without using the already defined versions.
--
-- This module is carefully structured so that it already compiles. You
-- can load it into GHCi right now! However, many functions in this module
-- are not actually implemented, but rather trigger a run-time error once
-- executed. Your task is it to replace all these uses of error by proper
-- definitions.
--
-- In the beginning, we import a number of modules. We hide some functions
-- from the Prelude, so that they won't conflict with the functions of the
-- same name we are going to define in this module. Also, the hiding serves
-- as an indicator for people with advance Haskell knowledge that you should,
-- for now, *not* use functions such as map, filter, foldr etc from the
-- libraries, but define everything via pattern matching.
--
-- Remember to test all your functions in GHCi! Make sure that the module
-- loads correctly into GHCi before submitting.

import           Prelude hiding (drop, filter, foldr, length, map, null, sum,
                          take)

-- Here is the rock-paper-scissors datatype from the introduction again.

data Choice = Rock | Paper | Scissors
  deriving (Show)

-- Task A-1:
--
-- Define using pattern matching a function 'isPaper' that checks if
-- the given argument is |Paper|.
--
-- Examples (in GHCi):
--
-- >>> isPaper Scissors
-- False
-- >>> isPaper Paper
-- True

isPaper :: Choice -> Bool
isPaper Paper = True
isPaper _     = False

-- The general structure for defining functions on lists is to follow
-- the structure of the list datatype. The list datatype has two
-- constructors of the following types:
--
--   []  :: [a]
--   (:) :: a -> [a] -> [a]
--
-- They're pronounced 'nil' and 'cons', respectively.
--
-- Following the structure of the datatype means:
--
--   * use one case per constructor (thus two cases),
--   * use pattern matching on the input,
--   * use recursion in the function wherever the datatype itself
--     is recursive (thus, use recursion on the tail of the list in
--     the cons-case).
--
-- The above is what we call the "standard design principle" for
-- functions.
--
-- From these guidelines, we obtain the following template for functions
-- on lists:
--
--   listFun :: [a] -> ...
--   listFun []       = ...
--   listFun (x : xs) = ... listFun xs ...
--
-- Try to use this template for all the following definitions. Sometimes,
-- there are shorter or more elegant solutions possible by combining and
-- reusing other functions. Nevertheless, this basic template is a good
-- strategy that you should always keep in mind.

-- Task A-2:
--
-- For the first list function, you won't actually need recursion.
--
-- The function 'null' should check if a list is empty. Define this
-- directly via pattern matching.

null :: [a] -> Bool
null [] = True
null _  = False

-- Task A-3:
--
-- Here are two "other" definitions of null. Why are they both worse
-- than the definition via pattern matching you've hopefully defined
-- above?
--
-- null2 xs = xs == []
-- null3 xs = length xs == 0
--
-- PLEASE ANSWER THE QUESTION HERE

-- Task A-4:
--
-- The function 'length' should determine the length of a list.
-- (Try to reimplement the function we've already seen without looking
-- at it ...)
--
-- Always try calling the functions in GHCi once you're done defining
-- them. E.g., see if the following expressions work as expected:
--
-- length []
-- length [1,2,3,4,5]
-- length [True, False]
-- length "foo"         -- strings are lists of characters in Haskell

length :: [a] -> Int
length []       = 0
length (_ : xs) = 1 + length xs

-- Task A-5:
--
-- The function 'noPaper' should check that no element of a list of
-- type 'Choice' is 'Paper'. You can reuse 'isPaper'.
--
-- Examples (in GHCi):
--
-- >>> noPaper []
-- True
-- >>> noPaper [Rock, Scissors, Rock]
-- True
-- >>> noPaper [Paper]
-- False
-- >>> noPaper [Rock, Scissors, Paper, Scissors, Rock]
-- False

noPaper :: [Choice] -> Bool
noPaper []       = True
noPaper (x : xs) = not $ isPaper x && noPaper xs

-- Task A-6:
--
-- The function 'sum' should sum all the entries in a list of integers.
--
-- Examples (in GHCi):
--
-- >>> sum []
-- 0
-- >>> sum [1,2,3]
-- 6
-- >>> sum [1,-1]
-- 0
-- >>> sum [1,99,-3,5]
-- 102

sum :: [Int] -> Int
sum []       = 0
sum (x : xs) = x + sum xs

-- Task A-7:
--
-- Here is our self-defined 'List' type again. Define functions
-- 'from' and 'to' converting between this type and the built-in lists.
--
-- For the user-defined lists, follow the equivalent design standard
-- design principle, with two cases, one for 'Nil' and one for 'Cons'.
--
-- Examples (in GHCi):
--
-- >>> from (Cons 1 (Cons 2 Nil))
-- [1,2]
-- >>> to [1,2]
-- Cons 1 (Cons 2 Nil)

data List a = Nil | Cons a (List a)
  deriving (Show)

from :: List a -> [a]
from Nil         = []
from (Cons x xs) = x : from xs

to :: [a] -> List a
to []       = Nil
to (x : xs) = Cons x (to xs)

-- Task A-8:
--
-- The function 'evens' should go through a list of integers and
-- produce a new list that only contains the even elements of the first
-- list.
--
-- Stick to the standard design principle for list functions.
--
-- You can use "if ... then ... else ..." to perform the test. There
-- already is a function
--
--   even :: Int -> Bool
--
-- available that performs the test and that you may use!
--
-- Examples (in GHCi):
--
-- >>> evens [1,2,3,4]
-- [2,4]
-- >>> evens [2,8,16,32]
-- [2,8,16,32]
-- >>> evens []
-- []

evens :: [Int] -> [Int]
evens [] = []
evens (x : xs)
  | even x = x : evens xs
  | otherwise = evens xs

-- Task A-9:
--
-- The function 'sumEvenSquares' should go through a list of integers,
-- keep only the even ones, square each of them, and compute the sum
-- of these.
--
-- Example (in GHCi):
--
-- >>> sumEvenSquares [1,2,3,4]
-- 20
--
-- (Because (2 * 2) + (4 * 4) = 20.)
--
-- Again, try to do this all following the standard principle, and in
-- one go. We will see later how we can achieve the same by combining
-- standard functions.

sumEvenSquares :: [Int] -> Int
sumEvenSquares [] = 0
sumEvenSquares (x : xs)
  | even x = x * x + sumEvenSquares xs
  | otherwise = sumEvenSquares xs

-- Task A-10:
--
-- The function 'allEven' should check whether all numbers in the
-- given list are even.
--
-- You can use the operator (&&) which implements logical "and".
--
-- Examples (in GHCi):
--
-- >>> allEven []
-- True
-- >>> allEven [2,4,6]
-- True
-- >>> allEven [1]
-- False
-- >>> allEven [1,2,3]
-- False

allEven :: [Int] -> Bool
allEven []       = True
allEven (x : xs) = even x && allEven xs

-- Task A-11:
--
-- The function 'isAscending' checks if a list of integers is
-- (strictly) increasing.
--
-- Hint: You need three cases rather than two for this function.
--
-- Examples (in GHCi):
--
-- >>> isAscending []
-- True
-- >>> isAscending [77]
-- True
-- >>> isAscending [66,55]
-- False
-- >>> isAscending [1,3,3,5]
-- False
-- >>> isAscending [1,3,5,7]
-- True
-- >>> isAscending [1,6,99,200,199,300]
-- False

isAscending :: [Int] -> Bool
isAscending (x : y : ys) = x < y && isAscending (y : ys)
isAscending _            = True

-- Task A-12:
--
-- The following datatype is a way to represent natural
-- numbers. Write a function by means of pattern matching
-- that converts such a natural number into its 'Int'
-- equivalent.
--
-- Examples (in GHCi):
--
-- >>> fromNat (Suc (Suc Zero))
-- 2
-- >>> fromNat Zero
-- 0
-- >>> fromNat (Suc (Suc (Suc (Suc (Suc Zero)))))
-- 5

data Nat = Zero | Suc Nat
  deriving (Show)

fromNat :: Nat -> Int
fromNat Zero    = 0
fromNat (Suc n) = 1 + fromNat n

-- Task A-13:
--
-- Define a function that adds two natural numbers.
--
-- Hint: You need to pattern match *only* on the first
-- argument. You need only two cases. The 'Nat' type is
-- recursive for 'Suc', so use recursion in that case.

add :: Nat -> Nat -> Nat
add Zero n    = n
add (Suc m) n = Suc (add m n)
