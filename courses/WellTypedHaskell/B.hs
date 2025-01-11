--------------------------------------------------------------------------
-- B.hs
--
-- (c) 2017-2024 Andres Loeh, Well-Typed LLP

{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}
module B where

-- We hide functions we are going to redefine.
import Prelude hiding (or, reverse, filter, replicate, repeat)
import Control.Applicative ((<|>), liftA2)
import Tables (orelse)

-- Task B-1.
--
-- The Either datatype represents a choice between two different
-- types, and is defined as follows:
--
-- data Either a b = Left a | Right b
--
-- Define a function 'collapse' that takes as 'Either [Int] Int'
-- and, if the value contained is a list, returns its sum (you
-- can use the existing 'sum' function), and if the value contained
-- is an integer, returns the integer itself.
--
-- Examples:
--
-- >>> collapse (Left [1,2,3])
-- 6
-- >>> collapse (Right 77)
-- 77
-- >>> collapse (Left [])
-- 0
--

collapse :: Either [Int] Int -> Int
collapse (Left [])       = 0
collapse (Left (x : xs)) = x + collapse (Left xs)
collapse (Right x)       = x

-- Task B-2.
--
-- The datatype 'Month' defined below is a simple wrapper around
-- an 'Int'. The 'newtype' construct works more or less like 'data',
-- but it works only in the special case that we are defining a
-- datatype with a single constructor (in this case 'MkMonth')
-- and a single argument (in this case of type 'Int'). It is
-- slightly more efficient than 'data' in this case, not introducing
-- an additional indirection, but ensuring that the run-time
-- representation of both a 'Month' and an 'Int' are exactly the
-- same.
--
-- But to Haskell, both types are different! We have to explicitly
-- apply
--
--   MkMonth :: Int -> Month
--
-- to turn an 'Int' into a 'Month', and we have to use pattern
-- matching to extract the 'Int', should we need it.
--
-- Now define a function
--
--   mkMonth :: Int -> Maybe Month
--
-- that constructs a 'Month' but only if it is valid, i.e., if
-- the given 'Int' is in the range of '1' to '12'. Otherwise,
-- the function should return 'Nothing'.
--
-- Examples:
--
-- >>> mkMonth 0
-- Nothing
-- >>> mkMonth 1
-- Just (MkMonth 1)
-- >>> mkMonth 7
-- Just (MkMonth 7)
-- >>> mkMonth 12
-- Just (MkMonth 12)
-- >>> mkMonth 13
-- Nothing
-- >>> mkMonth (-2)
-- Nothing
--

newtype Month = MkMonth Int
  deriving Show

mkMonth :: Int -> Maybe Month
mkMonth x
  | x <= 0 || x > 12 = Nothing
  | otherwise = Just (MkMonth x)

-- Task B-3.
--
-- Define a function 'mapMaybe' that maps a function over
-- a 'Maybe' value.
--
-- Examples:
--
-- >>> mapMaybe (+1) (Just 3)
-- Just 4
-- >>> mapMaybe (*2) Nothing
-- Nothing
-- >>> mapMaybe length (Just "hello")
-- Just 5
--
-- NOTE: This function is available in the libraries in more
-- general form under the name 'fmap', but you are supposed
-- to define it here from scratch using pattern matching.
--

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

-- Task B-4.
--
-- Implement a function that evaluates two 'Maybe's,
-- and if both are 'Just', returns their elements
-- as a pair. Otherwise, it returns 'Nothing'.
--
-- Define the function by pattern matching, without
-- using any other functions.
--
-- Examples:
--
-- >>> pairMaybe (Just 3) (Just 4)
-- Just (3,4)
-- >>> pairMaybe (Just 'x') Nothing
-- Nothing
-- >>> pairMaybe (Just "foo") (Just False)
-- Just ("foo", False)
-- >>> pairMaybe Nothing Nothing
-- Nothing
--

pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just x) (Just y) = Just (x, y)
pairMaybe _         _       = Nothing

-- Task B-5.
--
-- Define the function 'liftMaybe' that takes a
-- binary function and two possibly failing computations.
-- If both computations succeed, the function is used
-- to combine the results. If any of the computations
-- fail, then the result is 'Nothing'.
--
-- NOTE: This function is available in the libraries
-- in a more general form under the name 'liftA2',
-- but you're supposed to define it here from scratch
-- using pattern matching.
--
-- Examples:
--
-- >>> liftMaybe (+) (Just 3) (Just 7)
-- Just 10
-- >>> liftMaybe (+) (Just 3) Nothing
-- Nothing
-- >>> liftMaybe (\ x _ -> x) Nothing (Just 7)
-- Nothing
-- >>> liftMaybe (\ x _ -> x) (Just "foo") (Just "bar")
-- Just "foo"
-- >>> liftMaybe (++) (Just "foo") (Just "bar")
-- Just "foobar"

liftMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe f (Just x) (Just y) = Just (f x y) 
liftMaybe  _  _        _      = Nothing

-- Task B-6.
--
-- Reimplement 'pairMaybe' using 'liftMaybe' from the
-- previous task.
--
-- Do *not* use pattern matching here! This definition
-- should be significantly simpler than the original
-- definition of 'pairMaybe'.

pairMaybe' :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe' = liftMaybe (,) 

-- Task B-7.
--
-- Define a function lookups that performs lookups in a given
-- key-value table for every element in the list. Use the 'lookup'
-- function from the 'Prelude' that corresponds to the one that
-- we had on the slides.
--
-- If any of the lookups are failing, the whole function should
-- return 'Nothing'.
--
-- Examples:
--
-- >>> table = [(1, "x"), (2, "y"), (3, "z")]
-- >>> lookups [1,2,3] table
-- Just ["x", "y", "z"]
-- >>> lookups [3,1] table
-- Just ["z", "x"]
-- >>> lookups [2,2] table
-- Just ["y", "y"]
-- >>> lookups [] table
-- Just []
-- >>> lookups [7] table
-- Nothing
-- >>> lookups [1,2,3,4] table
-- Nothing
-- >>> lookups [2,0,2] table
-- Nothing
--

lookups :: Eq a => [a] -> [(a, b)] -> Maybe [b]
lookups [] _ = Just []
lookups (x : xs) table = liftMaybe (:) (lookup x table) (lookups xs table)

-- Smarter way to write lookups:
lookups' :: Eq a => [a] -> [(a, b)] -> Maybe [b]
lookups' xs table = traverse (\ x -> lookup x table) xs

-- Task B-8.
--
-- Reimplement the function 'reverse' from the slides,
-- using the "standard design principle on lists", i.e.,
-- using pattern matching and recursion, and the
-- function '++' in the recursive case.

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- Task B-9.
--
-- Implement a function that takes two lists and
-- returns the reversed second list concatenated with
-- the first list.
--
-- Do NOT use 'reverse' and '++' to define this function.
-- Do it directly, by applying the standard design
-- pattern on lists.
--
-- Examples:
--
-- >>> reverseAcc [1,2,3] []
-- [1,2,3]
-- >>> reverseAcc [3,2,1] [4,5,6]
-- [6,5,4,3,2,1]
--

reverseAcc :: [a] -> [a] -> [a]
reverseAcc xs [] = xs
reverseAcc xs ys = reverseAcc (head ys : xs) (tail ys)

-- Task B-10.
--
-- One way to look at the previous task is that the first
-- argument is the accumulator, that is initially empty,
-- and while traversing the list, contains the reversed
-- list that we have seen so far.
--
-- Observe that the reimplemented reverse below indeed
-- reverses a list.
--
-- Observe that '[1 .. 10]' produces a list containing
-- the numbers from '1' up to '10'.
--
-- Use this notation to generate lists of various lengths,
-- and then use the two versions of 'reverse' to reverse
-- them.
--
-- Do you observe one of the two versions to be faster
-- than the other? Why?
--
-- PLEASE ANSWER THE QUESTION HERE

reverse' :: [a] -> [a]
reverse' = reverseAcc []

-- Task B-11.
--
-- A call to 'replicate n x' should produce a list
-- containing n copies of x.
--
-- For negative numbers, it should behave as
-- 'replicate 0' does.
--
-- Examples:
--
-- >>> replicate 3 'x'
-- "xxx"
-- >>> replicate 5 False
-- [False, False, False, False, False]
-- >>> replicate 0 5
-- []
-- >>> replicate (-3) [1,2,3]
-- []
--

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0    = []
  | otherwise = x : replicate (n - 1) x

-- Task B-12.
--
-- We can also create an infinite list of copies of
-- one element using 'repeat'.
--
-- Example:
--
-- >>> repeat 1
-- [1,1,1,1,1,1,...
--
-- (continues until stopped using Ctrl-C)
--

repeat :: a -> [a]
repeat x = x : repeat x

-- Task B-13.
--
-- Redefine 'replicate' as a one-liner in terms of
-- 'repeat' and another list function you already know.
--

replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat x) 

-- Task B-14.
--
-- The type 'Tree' models binary trees that have some
-- information stored in the leaves, but not in the nodes.
--
-- It is defined as
--
--   data Tree a = Leaf a | Node (Tree a) (Tree a)
--
-- For example, the value
--
--   Node (Node (Leaf 2) (Leaf 4)) (Leaf 1)
--
-- can be imagined to look in ASCII art as follows:
--
--
--                   Node
--                   /  \
--                  /    \
--                 /      \
--               Node    Leaf 1
--               /  \
--              /    \
--             /      \
--           Leaf 2  Leaf 4
--
-- Implement a map function on trees. It
-- should produce a tree of the same shape
-- as the original tree, where the elements
-- in the new tree have been transformed by
-- the function.
--

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

tree1 :: Tree Int
tree1 = Leaf 1

tree2 :: Tree Int
tree2 = Node (Leaf 2) (Leaf 4)

tree3 :: Tree Int
tree3 = Node tree2 tree1

tree4 :: Tree Int
tree4 = Node tree2 tree3

tree5 :: Tree Int
tree5 = Node (Leaf 3) (Leaf 3)

tree6 :: Tree Int
tree6 = Node tree5 tree5

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)   = Leaf (f a)
mapTree f (Node a b) = Node (mapTree f a) (mapTree f b)


-- Task B-15.
--
-- Check whether two trees have the same
-- shape. Implement this directly, without
-- using other functions.
--
-- Examples:
--
-- >>> sameShape tree1 tree1
-- True
-- >>> sameShape tree3 tree2
-- False
-- >>> sameShape tree5 tree2
-- True
-- >>> sameShape tree3 tree3
-- True
--

sameShape :: Tree a -> Tree b -> Bool
sameShape (Leaf _)   (Leaf _)   = True
sameShape (Node a b) (Node c d) = sameShape a c && sameShape b d
sameShape _          _          = False

-- Task B-16.
--
-- Re-implement 'sameShape', but this time,
-- use the derived equality on trees and
-- 'mapTree'. Also use the pre-defined
-- "unit" datatype, which is a datatype with
-- just one value. It has special syntax and
-- can be thought to be defined via
--
--   data () = ()
--
-- The single constructor '()' is called
-- "unit" as well:
--
--   () :: ()

sameShape' :: Tree a -> Tree b -> Bool
sameShape' t1 t2 = mapTree (const ())  t1 == mapTree (const ()) t2

-- Task B-17.
--
-- Build a complete tree of the given height
-- (i.e., a tree where all leaves are at the
-- same depth from the root).
--
-- If the given integer is zero or negative,
-- return just a single leaf.
--
-- Examples:
--
-- >>> buildTree 0
-- Leaf ()
-- >>> buildTree 1
-- Node (Leaf ()) (Leaf ())
-- >>> buildTree 2
-- Node (Node (Leaf ()) (Leaf ())) (Node (Leaf ()) (Leaf ()))
-- >>> buildTree (-5)
-- Leaf ()
--

buildTree :: Int -> Tree ()
buildTree n 
  | n <= 0    = Leaf ()
  | otherwise = Node (buildTree (n - 1)) (buildTree (n - 1))

-- Task B-18.
--
-- The following datatype represent abstract syntax trees
-- of a simple expression language.
--
-- It contains literal integers ('Lit'), addition, unary
-- negation, and a special form of 'if-then-else' that checks
-- whether the first argument is 0, and depending on that
-- assumes the value of the second (if indeed 0) or the
-- third argument (if any other value).
--
-- Define a function
--
--   eval :: Expr -> Int
--
-- that evaluates an expression according to the intended
-- semantics.
--
-- Examples:
--
-- >>> prop_eval1
-- True
-- >>> prop_eval2
-- True
--

data Expr = 
    Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | IfZero Expr Expr Expr
  deriving (Eq, Show)

expr1 :: Expr
expr1 = Neg (Add (Lit 3) (Lit 5))

expr2 :: Expr
expr2 = IfZero expr1 (Lit 1) (Lit 0)

eval :: Expr -> Int
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Neg x)   = - eval x
eval (IfZero x y z)
  | eval x == 0 = eval y
  | otherwise   = eval z

prop_eval1 :: Bool
prop_eval1 = eval expr1 == -8

prop_eval2 :: Bool
prop_eval2 = eval expr2 == 0

-- Task B-19.
--
-- Implement a function that counts the number
-- of operations in an expression. All of 'Add',
-- 'Neg', and 'IfZero' count as one operation.
--
-- Examples:
--
-- >>> countOps expr1
-- 2
-- >>> countOps expr2
-- 3
--

countOps :: Expr -> Int
countOps (Lit _)        = 0
countOps (Add x y)      = 1 + countOps x + countOps y
countOps (Mul x y)      = 1 + countOps x + countOps y
countOps (Neg x)        = 1 + countOps x
countOps (IfZero x y z) = 1 + countOps x + countOps y + countOps z

-- Task B-20.
--
-- Add a constructor 'Mul' for multiplication to
-- the expression language and adapt all functions
-- accordingly.

-- Task B-21.
--
-- Let's re-visit the 'Tree' datatype from above.
--
-- Now define a new datatype 'Path' that
-- represents the sequence of steps one can walk
-- through such a binary tree of type 'Tree', from
-- the root to a potential leaf.
--
-- A path can either end, or we can go down to
-- the left subtree and continue, or we can go
-- down to the right subtree and continue -- so
-- there should be three constructors.

data Path = End | LeftT Path | RightT Path 
  deriving (Eq, Show)

-- Task B-22.
--
-- Define a function 'follow' that takes a path
-- and a tree and tries to look up the element
-- at the position described by the path.
--
-- For example, following the path "left, right"
-- in 'tree3' above should yield 'Just 4'.
--
-- Whereas following the paths "left" or
-- "left, left, right" in the same tree should
-- yield 'Nothing'.
--

follow :: Path -> Tree a -> Maybe a
follow End        (Leaf x)    = Just x
follow End        (Node _ _)  = Nothing
follow (LeftT x)  (Node y _)  = follow x y
follow (RightT x) (Node _ z)  = follow x z
follow _           _          = Nothing

-- Task B-23.
--
-- Define a function 'search' that tries to find
-- an element in a tree and returns the path.
--
-- If an element occurs more than once in the tree,
-- we want to find the leftmost occurrence.
--
-- (There is a very elegant solution for this using
-- functions we have already seen; defining this directly
-- by pattern matching, however, is quite hard.)
--

search :: Eq a => a -> Tree a -> Maybe Path
search x (Leaf y)
  | x == y = Just End
  | otherwise = Nothing
search x (Node l r) = (LeftT <$> search x l) <|> (RightT <$> search x r)
