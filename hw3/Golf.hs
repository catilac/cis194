{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Hopscotch

-- every :: Int -> [a] -> [a]
-- every n l = map fst [x | x <- zip l [1..], (snd x) `mod` n == 0]

skips :: [a] -> [[a]]
skips l = map (flip every l ) [1..length(l)]
  where
    every n lst = map fst [x | x <- zip lst [1..], (snd x) `mod` n == 0]
