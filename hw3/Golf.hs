{-# OPTIONS_GHC -Wall #-}
module Golf where

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
