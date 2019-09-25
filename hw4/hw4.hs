{-# OPTIONS_GHC -Wall #-}

module Wholemeal where

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
