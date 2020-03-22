module Adversity where

import Data.Maybe (maybe)

-- Determine the kinds
-- 1. Given id :: a -> a
-- :k a = *
-- 2. r :: a -> f a
-- :k a = *
-- :k f a = *
-- :k f = * -> *
-- String processing
-- 1.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . map (maybe "a" id . notThe) .  words

-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input = sum $ zipWith matchTHe notThes (tail notThes)
  where notThes = fmap (notThe) $ words input
        matchTHe Nothing (Just s) = if isVowel (head s) then 1 else 0
        matchTHe _ _ = 0
        isVowel = flip elem "aeiou"
-- 3.
isVowel = flip elem "aeiou"
countVowels :: String -> Int
countVowels = length . filter isVowel

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)
mkWord :: String -> Maybe Word'
mkWord word = if valid then Just $ Word' word else Nothing
  where valid = countVowels word < length word - countVowels word

-- It’s only Natural
data Nat = Zero | Succ Nat
  deriving (Eq, Show)
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x = if valid then Just $ convert x else Nothing
  where valid = x > 0
        convert 0 = Zero
        convert x = Succ (convert $ x-1)

-- Small library for Maybe
-- 1.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def f Nothing = def
mayybee def f (Just x) = f x

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe def (Just x) = x

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

--5.
catMaybes :: [Maybe a] -> [a]
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x): xs) = x : catMaybes xs

-- 6.
flipMaybe :: Eq a => [Maybe a] -> Maybe [a]
flipMaybe xs = if Nothing `elem` xs then Nothing else Just $ catMaybes xs

-- Small library for Either
-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr (\val acc -> if isLeft val then (fromLeft val):acc else acc) []
  where
    isLeft (Left _) = True
    isLeft _ = False
    fromLeft (Left x) = x

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr (\val acc -> if isRight val then (fromRight val):acc else acc) []
  where
    isRight (Right _) = True
    isRight _ = False
    fromRight (Right x) = x

Homework: https://gist.github.com/K0Te/8ca67a84e038240c1dee8cf77601ee68
It's not 100% ready, sorry. I've overestimated my skills and ran out of time...