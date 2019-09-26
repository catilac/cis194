{-# OPTIONS_GHC -Wall #-}

module Wholemeal where

-- Wholemeal coding 

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x = (x - 2)*fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldr (*) 1 (map (\x -> x - 2) (filter even xs))

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = foldr (+) 0 $
          filter even $
          takeWhile (> 1) $
          iterate (\x ->  if even x then (x `div` 2) else 3*x+1) n

-- Folding
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Returns a balanced binary tree
-- Balanced binary tree is balanced if:
--  1. Left and Right trees are balanced
--  2. The height between the right and left subtrees is <= 1
-- foldTree :: [a] -> Tree a
-- foldTree xs = foldr insert Leaf xs

-- insert :: a -> Tree a -> Tree a
-- insert v Leaf = Node 0 Leaf v Leaf
-- 
-- insert v (Node _ Leaf v' Leaf) = Node 1 Leaf v' (Node 0 Leaf v Leaf)
-- insert v (Node _ Leaf v' right) = Node 1 (Node 0 Leaf v Leaf) v' right
-- insert v (Node _ left v' Leaf) = Node 1 left v' (Node 0 Leaf v Leaf)
-- 
-- insert v (Node h left v' right)
--   -- when do we increase the height?
--   | hl > hr = Node (h+1)-- insert right?
--   | otherwise = -- insert left?
--   where
--     Node hl _ _ _ = left
--     Node hr _ _ _ = right



-- xor returns True iff there are an
-- odd number of True values in the input list
-- it does not matter how many False values
-- are in the list
xor :: [Bool] -> Bool
xor bs = foldr (/=) False bs

-- implement map as a fold
-- map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr f 
