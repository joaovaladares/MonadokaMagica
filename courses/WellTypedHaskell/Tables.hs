{-# OPTIONS_GHC -Wall #-}

module Tables where

import           Prelude hiding (curry, fst, snd, uncurry, zip, zipWith, lookup)

data Pair a b  = Pair a b
    deriving Show

fst :: Pair a b -> a
fst (Pair x _) = x

fst' :: (a, b) -> a
fst' (x, _) = x

snd :: Pair a b -> b
snd (Pair _ y) = y

snd' :: (a, b) -> b
snd' (_, y) = y

fromPair :: Pair a b -> (a, b)
fromPair (Pair x y) = (x, y)

toPair :: (a, b) -> Pair a b
toPair (x, y) = Pair x y

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

swap' :: Pair a b -> Pair b a
swap' (Pair x y) = Pair y x

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip  _        _       = []

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
zipWith _  _         _      = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = map (uncurry f) (zip xs ys)

type Table key val = [(key, val)]

lookup :: Show key => Eq key => key -> [(key, val)] -> val
lookup key [] = error $ "Key " ++ show key ++ " not found"
lookup key ((key', val) : table)
  | key == key' = val
  | otherwise   = lookup key table

lookup' :: Show key => Eq key => key -> Table key val -> Maybe val
lookup' _ [] = Nothing
lookup' key ((key', val) : table)
  | key == key' = Just val
  | otherwise   = lookup' key table

delete :: Eq key => key -> Table key val -> Table key val 
delete _ [] = []
delete key ((key', val) : table)
  | key == key' = delete key table 
  | otherwise   = (key', val) : delete key table

delete' :: Eq key => key -> Table key val -> Table key val
delete' key = filter (\ (x,_) -> key /= x)

testTable :: [(Int, Int)]
testTable =
  [ (1, 4)
  , (2, 5)
  , (3, 6)
  , (3, 10)
  , (4, 7)
  ]

safeHead :: [a] -> Maybe a
safeHead      [] = Nothing
safeHead (x : _) = Just x

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum []       = Nothing
safeMaximum (x : xs) = Just $ maximumAux x xs 

maximumAux :: Ord a => a -> [a] -> a 
maximumAux x []       = x
maximumAux x (y : ys) = maximumAux (max x y) ys

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing  = def
fromMaybe _   (Just x) = x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b -- available more generally as fmap or (<$>)
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

orelse :: Maybe a -> Maybe a -> Maybe a -- available more generally as (<|>)
orelse Nothing y  = y
orelse (Just x) _ = Just x

safeExtract :: String -> Char
safeExtract str = fromMaybe ' ' $ safeHead str

alts :: [Maybe a] -> Maybe a
alts []             = Nothing
alts (Nothing : xs) = alts xs 
alts (Just x : _)   = Just x

