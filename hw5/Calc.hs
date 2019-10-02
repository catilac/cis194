{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser(parseExp)
import StackVM

-- eval :: ExprT -> Integer
-- eval (Lit n) = n :: Integer
-- eval (Add lexpr rexpr) = eval lexpr + eval rexpr
-- eval (Mul lexpr rexpr) = eval lexpr * eval rexpr
-- 
-- evalStr :: String -> Maybe Integer
-- evalStr s = case (parseExp Lit Add Mul s) of
--   Just expr -> Just (eval expr)
--   Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

-- instance Expr ExprT where
--   lit = Lit
--   add = Add
--   mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 . (`mod` 7) $ a + b
  mul (Mod7 a) (Mod7 b) = Mod7 . (`mod` 7) $ a * b

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

-- Exercise 5
-- stackVM exp == Right [IVal exp]
instance Expr Program where
  lit = (:[]) . PushI
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
