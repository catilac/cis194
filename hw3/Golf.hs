{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

-- Hopscotch

skips :: [a] -> [[a]]
skips l = map (every l) [1..length(l)]
  where
    every lst n = map fst [x | x <- zip lst [1..], (snd x) `mod` n == 0]

-- Local Maxima

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x:y:z:ns)
  | x < y && z < y = y:localMaxima(z:ns)
  | otherwise = localMaxima(y:z:ns)

-- Histogram

histogram :: [Integer] -> String
histogram ns =
  (buildGraph ns) ++ intercalate "\n" [border, indices]
  where
    buildGraph :: [Integer] -> String
    buildGraph [] = ""
    -- just take one instance of every number out and graph it
    buildGraph xs = buildGraph(xs \\ (nub xs))  ++ row xs  ++ "\n"
    border = (take 10 (repeat '='))
    indices = "0123456789"

    row :: [Integer] -> String
    row [] = ""
    -- check if every integer 0..9 is inside of the list of nums
    -- plot a * if it's in there, otherwise a space
    row nums = map (plot . (flip elem nums)) [0..9] 
       where
        plot :: Bool -> Char
        plot True = '*'
        plot _ = ' '

