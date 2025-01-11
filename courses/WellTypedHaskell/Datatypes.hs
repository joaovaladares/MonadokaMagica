{-# OPTIONS_GHC -Wall #-}

module Datatypes where

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)

johnDoe :: Person
johnDoe = Person
  { name = "John Doe"
  , age  = 30
  }
