{-# LANGUAGE BangPatterns #-}
---------------------------------------------------------------------
-- C.hs
--
-- (c) 2017-2024 Andres Loeh, Well-Typed LLP
--
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module C where

import Data.Char (isSpace, toUpper)
import Data.List (foldl')
import Data.Function ((&))
import Prelude hiding (concat, product, take)

-- Task C-1.
--
-- Define 'product' using an explicit strict accumulator.
--

product :: Num a => [a] -> a
product = productAux 1

productAux :: Num a => a -> [a] -> a
productAux !acc []       = acc
productAux !acc (x : xs) = productAux (acc * x) xs


-- Task C-2.
--
-- Define 'product' using a suitable fold (i.e., decide whether it
-- should be defined using foldl' or foldr).
--

product' :: Num a => [a] -> a
product' = foldl' (*) 1

-- Task C-3.
--
-- In a list of strings, find how many strings consist purely
-- of whitespace. You can use the function
--
--   isSpace :: Char -> Bool
--
-- from the Data.Char module to determine if a single character
-- is a whitespace character.
--
-- Define this function as a composition of other functions!
--
-- Examples:
--
-- >>> countWhiteSpaceStrings ["  x ", "  ", ""]
-- 2
-- >>> countWhiteSpaceStrings ["  x "]
-- 0
-- >>> countWhiteSpaceStrings []
-- 0
-- >>> countWhiteSpaceStrings [""]
-- 1
-- >>> countWhiteSpaceStrings ["a", "b", "c", "d"]
-- 0
-- >>> countWhiteSpaceStrings ["foo", "   ", "bar", " ", "baz", "   ", "      "]
-- 4
--

countWhiteSpaceStrings :: [String] -> Int
countWhiteSpaceStrings = length . filter (all isSpace)

-- Task C-4.
--
-- Remove all the words with more than the given number of letters.
-- You can use the functions
--
--   words :: String -> [String]
--   unwords :: [String] -> String
--
-- to split the original sentence into words and to turn back a list
-- of words into a sentence. The function is allowed to collapse
-- whitespace.
--
-- Define this function as a composition of other functions!
--
-- Examples:
--
-- >>> removeLongWords 8 "Haskell is a wonderful language"
-- "Haskell is a language"
-- >>> removeLongWords 2 "Haskell is a wonderful language"
-- "is a"
-- >>> removeLongWords 1 "Haskell is a wonderful language"
-- "a"
-- >>> removeLongWords 0 "Haskell is a wonderful language"
-- ""
-- >>> removeLongWords 9 "  Superfluous  whitespace    is removed   "
-- "whitespace is removed"
-- >>> removeLongWords 14 "  Superfluous  whitespace    is removed   "
-- "Superfluous whitespace is removed"
--

removeLongWords :: Int -> String -> String
removeLongWords n str = unwords . filter ((<= n) . length) $ words str


-- Task C-5.
--
-- Define the function
--
--   concat :: [[a]] -> [a]
--
-- that concatenates (flattens) a list of lists
-- using a suitable fold (i.e., decide whether it
-- should be defined using foldl' or foldr).
--
-- Examples:
--
-- >>> concat [[1,2,3], [4,5], [], [6,7]]
-- [1,2,3,4,5,6,7]
-- >>> concat []
-- []
-- >>> concat [[]]
-- []
-- >>> concat ["hello", "world"]
-- "helloworld"
--

concat :: [[a]] -> [a]
concat = foldl' (++) []

-- Task C-6.
--
-- Below is a definition for an enumeration type 'Dir' of directions,
-- and a definition of a record type 'Pos' of positions. The position
-- type stores how many steps we are "up" and "right" from an 'origin'
-- point where both coordinates are 0.
--
-- The exclamation marks in both components of the record type make
-- the record strict in both fields.
--
-- First define a function
--
--   goDir :: Pos -> Dir -> Pos
--
-- the maps a direction into a modification of a position.
--
-- Then define a function
--
--   goDirs :: Pos -> [Dir] -> Pos
--
-- that performs many movements in sequence from an initial position
-- and yields the end position.
--
--
-- MkPos {pUp = 5, pRight = 4}
-- Do this using a strict fold. It is important here that our 'Pos'
-- type has strict components. If it had not, even the forced evaluation
-- of the strict fold would only evaluate up to the constructor of the
-- 'Pos' type, but not force its components.
-- You can observe this by (temporarily!) removing the strictness
-- annotations from the 'Pos' type and running the final test case
-- for 'goDirs'. It should still succeed, but have noticeable memory
-- use.
--
-- Examples:
--
-- >>> goDir origin Up
-- MkPos {pUp = 1, pRight = 0}
-- >>> goDir (MkPos 5 5) Lft
-- >>> goDir (MkPos (-3) 0) Down
-- MkPos {pUp = -4, pRight = 0}
--
-- >>> goDirs origin [Up,Rgt,Down,Lft]
-- MkPos {pUp = 0, pRight = 0}
-- >>> goDirs origin [Rgt,Rgt,Rgt,Down,Down]
-- MkPos {pUp = -2, pRight = 3}
-- >>> goDirs (MkPos (-2) 3) [Lft,Lft,Lft,Up,Up]
-- MkPos {pUp = 0, pRight = 0}
-- >>> goDirs (MkPos 1 2) []
-- MkPos {pUp = 1, pRight = 2}
-- >>> goDirs origin (replicate 10000000 Up)
-- MkPos {pUp = 10000000, pRight = 0}
--

data Dir = Up | Rgt | Down | Lft
  deriving (Show)

data Pos = MkPos {pUp :: !Int, pRight :: !Int}
  deriving (Show)

origin :: Pos
origin = MkPos 0 0

goDir :: Pos -> Dir -> Pos
goDir (MkPos up right) = go
  where
    go Up   = MkPos (up + 1) right 
    go Down = MkPos (up - 1) right
    go Rgt  = MkPos up (right + 1)
    go Lft  = MkPos up (right - 1)

goDir' :: Pos -> Dir -> Pos
goDir' (MkPos up right) dir = case dir of
    Up   -> MkPos (up + 1) right
    Down -> MkPos (up - 1) right
    Rgt  -> MkPos up (right + 1)
    Lft  -> MkPos up (right - 1)

goDirs :: Pos -> [Dir] -> Pos
goDirs = foldl' goDir

-- Task C-7.
--
-- Define a function 'inits' that computes all prefixes of
-- a given list in order of increasing length, starting with
-- the empty prefix.
--
-- Define this function a suitable fold (i.e., decide whether it
-- should be defined using foldl' or foldr).
--
-- Examples:
--
-- >>> inits [1,2,3]
-- [[],[1],[1,2],[1,2,3]]
-- >>> inits "Haskell"
-- ["","H","Ha","Has","Hask","Haske","Haskel","Haskell"]
-- >>> inits []
-- [[]]
--

inits :: [a] -> [[a]]
inits = foldr (\x r -> [] : map (x:) r) []

-- These are binary trees with labels in their nodes.

-- Task C-8.
--
-- The 'BinTree' type define a different form of binary
-- trees where labels are stored in the nodes.
--
-- There are also some example trees.
--
-- Define a map function on such binary trees called
-- 'mapBinTree'. Provide the type signature yourself.
--
-- Examples:
--
-- >>> mapBinTree (: []) tree3
-- Bin (Bin Empty "x" Empty) "a" (Bin Empty "y" Empty)
-- >>> mapBinTree toUpper tree4
-- Bin (Bin (Bin Empty 'X' Empty) 'A' (Bin Empty 'Y' Empty)) 'B' (Bin Empty 'X' Empty)
-- >>> mapBinTree (const 3) tree5
-- Bin Empty 3 (Bin (Bin (Bin Empty 3 Empty) 3 (Bin Empty 3 Empty)) 3 (Bin Empty 3 Empty))
-- >>> mapBinTree (+1) Empty
-- Empty
--

data BinTree a
  = Bin (BinTree a) a (BinTree a)
  | Empty
  deriving (Eq, Show)

tree1 :: BinTree Char
tree1 = Bin Empty 'x' Empty

tree2 :: BinTree Char
tree2 = Bin Empty 'y' Empty

tree3 :: BinTree Char
tree3 = Bin tree1 'a' tree2

tree4 :: BinTree Char
tree4 = Bin tree3 'b' tree1

tree5 :: BinTree Char
tree5 = Bin Empty 'c' tree4

tree6 :: BinTree Char
tree6 = Bin tree5 'd' tree4

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree _ Empty = Empty
mapBinTree f (Bin l a r) = Bin (mapBinTree f l) (f a) (mapBinTree f r)


-- Task C-9.
--
-- We want to attach unique numbers to each node in a binary
-- tree, so that all the numbers from left to right are labelled
-- in ascending order. The first number used should be 1.
--
-- NOTE: You may have to define a more general helper function
-- in order to succeed here. Think about what information you
-- need to know in general before and after labelling a subtree,
-- and then try to define a function that computes the necessary
-- outputs from the necessary inputs.
--
-- There is a HINT at the end of this file if you need a bit
-- more guidance.
--
-- Examples:
--
-- >>> labelTree Empty
-- Empty
-- >>> labelTree tree1
-- Bin Empty ('x',1) Empty
-- >>> labelTree tree4
-- Bin (Bin (Bin Empty ('x',1) Empty) ('a',2) (Bin Empty ('y',3) Empty)) ('b',4) (Bin Empty ('x',5) Empty)
-- >>> labelTree tree6
-- Bin (Bin Empty ('c',1) (Bin (Bin (Bin Empty ('x',2) Empty) ('a',3) (Bin Empty ('y',4) Empty)) ('b',5) (Bin Empty ('x',6) Empty))) ('d',7) (Bin (Bin (Bin Empty ('x',8) Empty) ('a',9) (Bin Empty ('y',10) Empty)) ('b',11) (Bin Empty ('x',12) Empty))
--

labelTree :: BinTree a -> BinTree (a, Int)
labelTree = fst . labelTreeAux 1

labelTreeAux :: Int -> BinTree a -> (BinTree (a, Int), Int)
labelTreeAux !acc Empty = (Empty, acc)
labelTreeAux !acc (Bin l a r) =
  let (l', acc') = labelTreeAux acc l 
      (r', acc'') = labelTreeAux (acc' + 1) r
  in (Bin l' (a, acc') r', acc'')

-- Task C-10.
--
-- Try to implement the function 'take' on lists as an application
-- of 'foldr'.
--
-- The original definition of 'take' using pattern matching is
-- provided for reference.
--
-- Consider the type signature and a possible definition of
-- take, and note that not just the list is being traversed,
-- but also the number changes.
--
-- There is a HINT at the end of this file if you need a bit
-- more guidance.
--

take :: Int -> [a] -> [a]
take _ [] = []
take n (x : xs)
  | n > 0 = x : take (n - 1) xs
  | otherwise = []

take' :: Int -> [a] -> [a]
take' n xs = takeAux xs n

takeAux :: [a] -> (Int -> [a])
takeAux = foldr (\x r n -> if n == 0 then [] else x : r (n - 1)) (\_ -> [])

-- HINTS for some of the tasks
--
--
--
-- HINT for C-9.
--
-- In general, you will need to know for each subtree what
-- the first label you can use is. But to obtain this, you
-- will also need to know for each subtree after labelling
-- it what the next label you can use is.
--
-- So try to define a helper function:
--
-- labelTreeAux :: Int -> BinTree a -> (BinTree (a, Int), Int)
--
-- The first argument indicates the next "free" number.
-- The returned integer should indicate the next "free" number
-- after labelling of the given tree is complete.
--
-- With this, you should be able to apply the standard
-- design pattern on binary trees, but you will still have
-- to use let or case constructs to deconstruct the
-- pairs resulting from the recursive calls.
--
-- HINT for C-10.
--
-- The key insight here is that you can use 'foldr' in such
-- a way that the result is a function.
--
-- If the above hint is not enough for you, go on for more
-- detail:
--
-- You can imagine 'take' with its arguments flipped:
--
-- takeAux :: [a] -> (Int -> [a])
--
-- Now you can try to define 'take' in terms of foldr as
-- follows:
--
-- takeAux xs = foldr _1 _2 xs
--
-- The two holes now must have the following types:
--
-- _2 :: Int -> [a]
--
-- This corresponds to the case for the empty list. In dependence
-- of the numeric argument to 'take', you have to say which list
-- to return if the input list is empty. That should be easy.
--
-- _1 :: a -> (Int -> [a]) -> (Int -> [a])
--
-- Here you get two arguments, corresponding to the cons-case:
-- the next list element in question, and a partially applied
-- recursive case, where you know you'll take from the tail, but
-- you can still specify how many elements you want to take.
--
-- Finally, you also have to produce a function that will
-- receive the number of elements desired and has to produce the
-- correct list.
--

