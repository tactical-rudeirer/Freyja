{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Main where

nil = undefined

data Nil
data Cons x xs

class First list x | list -> x
instance First Nil Nil
instance First (Cons x more) x

class ListConcat as bs cs | as bs -> cs
instance ListConcat Nil bs bs
instance (ListConcat as bs cs)
  => ListConcat (Cons a as) bs (Cons a cs)


-- Concatenate all lists in a list
class ListConcatAll ls l | ls -> l
instance ListConcatAll Nil Nil
instance (ListConcat firstlist acc result,
          ListConcatAll rest acc)
  => ListConcatAll (Cons firstlist rest) result

data True 
data False 

instance Show True where
    show _ = "True"

instance Show False where
    show _ = "False"

-- Is any element of this list True?
class AnyTrue list t | list -> t
instance AnyTrue Nil              False
instance AnyTrue (Cons True more) True
instance (AnyTrue list t)
  => AnyTrue (Cons False list) t

class Not b1 b | b1 -> b
instance Not False True
instance Not True  False

class Or b1 b2 b | b1 b2 -> b
instance Or True  True  True
instance Or True  False True
instance Or False True  True
instance Or False False False


data Z
data S n
 
class ToInt t
    where toInt :: t -> Int

instance ToInt Z where
    toInt _ = 0
instance TypeBack (S x) x => ToInt (S x) where
    toInt sx =  1 + (toInt $ back sx)

class ToInt b => TypeBack a b | a -> b where
    back :: a -> b

instance ToInt x => TypeBack (S x) x where
    back _ = undefined


type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7

class Pred a b | b -> a
instance Pred x (S x)

-- Equality
class PeanoEqual a b t | a b -> t
instance PeanoEqual Z     Z     True
instance PeanoEqual (S a) Z     False
instance PeanoEqual Z     (S b) False
instance (PeanoEqual a b t)
  => PeanoEqual (S a) (S b) t

-- Comparison (<)
class PeanoLT a b t | a b -> t
instance PeanoLT Z      Z     False
instance PeanoLT (S x)  Z     False
instance PeanoLT Z      (S x) True
instance (PeanoLT a b t)
  => PeanoLT (S a) (S b) t

-- Absolute difference
class PeanoAbsDiff a b c | a b -> c
instance PeanoAbsDiff Z Z Z
instance PeanoAbsDiff Z (S b) (S b)
instance PeanoAbsDiff (S a) Z (S a)
instance (PeanoAbsDiff a b c)
  => PeanoAbsDiff (S a) (S b) c

-- Integers from n to 0
class Range n xs | n -> xs
instance Range Z Nil
instance (Range n xs)
  => Range (S n) (Cons n xs)

class LegalCompare t | -> t
  where test :: t
instance (PeanoEqual (S (S Z)) (S Z) t)
  => LegalCompare t

acht :: N8
acht = undefined

main :: IO ()
main = putStrLn $ show $ "hi" --toInt acht