{-# LANGUAGE FlexibleInstances #-}

module Cars where

import Data.Char
import Data.Maybe

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' ((Left x):xs) = x : lefts' xs
lefts' ((Right x):xs) = lefts' xs

getLeft :: Either a b -> [a] -> [a]
getLeft (Left x) acc = x:acc
getLeft (Right _) acc = acc

lefts'' :: [Either a b] -> [a]
lefts'' = foldr getLeft []

getRight :: Either a b -> [b] -> [b]
getRight (Right x) acc = x:acc
getRight (Left _) acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr getRight []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = ((lefts'' x), (rights' x))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' f _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa fb (Right x) = fb x
either' fa fb (Left x) = fa x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (\x -> Just $ f x)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = if isNothing res then [] else (getA res): myUnfoldr f (getB res)
    where res = f b
          getA (Just (a, b)) = a
          getB (Just (a, b)) = b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = if isNothing res then Leaf
    else Node (unfold f (getFst res)) (getSnd res) (unfold f (getThird res))
    where res = f a
          getFst (Just (a, b, c)) = a
          getSnd (Just (a, b, c)) = b
          getThird (Just (a, b, c)) = c

mkNode :: Int -> Maybe (Int, Int, Int)
mkNode 0 = Nothing
mkNode x = Just (x-1, x-1, x-1)

treeBuild :: Int -> BinaryTree Int
treeBuild n = unfold mkNode n