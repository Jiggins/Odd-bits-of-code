{-  Haskell without the Haskell
    This was just a little experiment to see if I it was possible to write the 
    quicksort algorithm in Haskell without using any of the standard library 
    functions, including <=, > and even $.  I reimplemented all of the 
    functions, classes and instances needed for quicksort.
-}

-- This Language Pragma prevents any of the standard library functions from
-- being imported.
{-# LANGUAGE NoImplicitPrelude #-}

module NoPrelude where

import System.IO (putStrLn) -- I'm not doing any IO stuff
import GHC.Types (Char)
-- I am importing Char here because Char is defined at a very low level in
-- Haskell

data Ordering = LT | EQ | GT
data Bool  = False | True -- Booleans are fun

-- * Classes and Instances

-- | Implementing Equality
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    x == y = not (x /= y)
    x /= y = not (x == y)

not :: Bool -> Bool
not True = False
not False = True

-- | Implementing the Ord class, allowing for <=, <, >, >= functions.
class Eq a => Ord a where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool

    x <= y = compare x y /= GT
    x <  y = compare x y == LT
    x >= y = compare x y /= LT
    x >  y = compare x y == GT

instance Eq Bool where
    True  == True  = True
    False == False = True
    _ == _ = False

instance Eq Ordering where
    LT == LT = True
    EQ == EQ = True
    GT == GT = True
    _ == _   = False

instance Ord Bool where
    compare False False = EQ
    compare False True  = LT
    compare True  False = GT
    compare True  True  = EQ

-- | Implementing ordering of list structures.  Uses a recursive compare 
-- function.
-- Note: Guards and if statements are bound to the GHC.Types.Bool class,
--       so I had to use case statements to compare things.
instance (Eq a, Ord a) => Ord ([a]) where
    [] `compare` _  = LT
    _  `compare` [] = GT
    (x:xs) `compare` (y:ys) = case x `compare` y of
        LT -> LT
        GT -> GT
        EQ -> xs `compare` ys

-- | Are two lists equal?
instance Eq a => Eq ([a]) where
    [] == [] = True
    [] == _  = False
    _  == [] = False
    (x:xs) == (y:ys) = case x == y of
        True -> True
        False -> xs == ys

-- | For printing
class Show a where
    show :: a -> String

-- | A string is just a list of characters
type String = [Char]

instance Show a => Show ([a]) where
    show [] = "[]"
    show (x:xs) = '[' : show x ++ (concat $ map ((',':) . show) xs) ++ "]"

instance Show Char where
    show = (:[])

instance Show Bool where
    show True = "True"
    show False = "False"

instance Show Ordering where
    show LT = "LT"
    show EQ = "EQ"
    show GT = "GT"

-- * Operators!

-- | List Concatenation
(++) :: [a] -> [a] -> [a]
[] ++ [] = []
[] ++ (y:ys) = y : ([] ++ ys)
(x:xs) ++ ys = x : (xs ++ ys)

-- | Function application
($) :: (a -> b) -> a -> b
f $ x = f x

-- | Function Composition
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- * Common haskell functions

filter :: (a -> Bool) -> [a] -> [a] 
filter _ [] = []
filter f (x:xs) = case f x of
    True  -> x : filter f xs
    False ->     filter f xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- | Concat a list of lists, or a list of Strings
concat :: [[a]] -> [a]
concat = foldl (++) []

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a xs = fold a xs
    where fold a [] = a
          fold a (x:xs) = (f a x) `seq` fold (f a x) xs
                        -- Using the strict version of foldl
                        -- seq causes (f a x) to be evaluated before
                        -- making a recursive function call.

-- | Equivalent of ⊥
-- Makes foldl strict and usually more efficient.
seq :: a -> b -> b
seq a b = b

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)

print = putStrLn . show
main = print $ quicksort [True, False]
