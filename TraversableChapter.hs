{-# LANGUAGE FlexibleContexts #-}
module TraversableChapter where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Foldable (foldMap)
import Data.Monoid

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity x) = Identity $ f x
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> x = fmap f x
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq
instance Foldable Identity where
  foldMap f (Identity x) = f x
instance Traversable Identity where
  traverse fa (Identity x) = fmap Identity $ fa x

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap f (Constant x) = Constant x
instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary
instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq
instance Foldable (Constant a) where
  foldMap f (Constant x) = mempty
instance Traversable (Constant a) where
  traverse fa (Constant x) = pure $ Constant x

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)
instance Functor (Optional) where
  fmap f (Yep x) = Yep $ f x
  fmap f Nada = Nada
instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]
instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq
instance Foldable (Optional) where
  foldMap f (Nada) = mempty
  foldMap f (Yep x) = f x
instance Traversable (Optional) where
  traverse fa Nada = pure Nada
  traverse fa (Yep x) = fmap Yep $ fa x

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)
instance Functor (List) where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil,
                     do x <- arbitrary
                        return (Cons x Nil),
                     do x1 <- arbitrary
                        x2 <- arbitrary
                        return (Cons x1 (Cons x2 Nil))
                    ]
instance (Eq a) => EqProp (List a) where
  (=-=) = eq
instance Foldable (List) where
  foldMap f (Nil) = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs
instance Traversable (List) where
  sequenceA Nil = pure Nil
  sequenceA (Cons a as) = pure (\x rst -> Cons x rst) <*> a <*> rest
    where rest = (sequenceA as)

data Three a b c = Three a b c deriving (Eq, Ord, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
instance Traversable (Three a b) where
  sequenceA (Three a b fc) = fmap (\x -> Three a b x) fc

data Big a b = Big a b b deriving (Eq, Ord, Show)
instance Functor (Big a) where
  fmap f (Big a x y) = Big a (f x) (f y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    x <- arbitrary
    y <- arbitrary
    return $ Big a x y
instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq
instance Foldable (Big a) where
  foldMap f (Big a x y) = (f x) <> f y
instance Traversable (Big a) where
  sequenceA (Big a fx fy) = pure (\x y -> Big a x y) <*> fx <*> fy

data S n a = S (n a) a deriving (Eq, Show)
instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)
instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = do
    na <- arbitrary
    a <- arbitrary
    return $ S na a
instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq
instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = f a <> (foldMap f na)
instance (Traversable n) => Traversable (S n) where
  sequenceA (S na a) = pure (\x xs -> S xs x) <*> a <*> (sequenceA na)


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)
instance Functor (Tree) where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof [return Empty, Leaf <$> arbitrary, genTree]
    where
      genTree = do
        left <- arbitrary
        right <- arbitrary
        a <- arbitrary
        return $ Node left a right
instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq
instance Foldable (Tree) where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)
instance Traversable (Tree) where
  sequenceA (Empty) = pure Empty
  sequenceA (Leaf a) = fmap Leaf a
  sequenceA (Node left a right) = pure (\l x r -> Node l x r) <*> (sequenceA left) <*> a <*> (sequenceA right)

main = do
  let trigger = undefined :: Identity (Int, Int, [Int])
  quickBatch $ functor trigger
  quickBatch $ traversable trigger

  let constant = undefined :: Constant Int (Int, Int, [Int])
  quickBatch $ functor constant
  quickBatch $ traversable constant

  let maybe = undefined :: Optional (Int, Int, [Int])
  quickBatch $ functor maybe
  quickBatch $ traversable maybe

  let lst = undefined :: List (Int, Int, [Int])
  quickBatch $ functor lst
  quickBatch $ traversable lst

  let three = undefined :: Three Int Int (Int, Int, [Int])
  quickBatch $ functor three
  quickBatch $ traversable three

  let big = undefined :: Big Int (Int, Int, [Int])
  quickBatch $ functor big
  quickBatch $ traversable big

  let sa = undefined :: S [] (Int, Int, [Int])
  quickBatch $ functor sa
  quickBatch $ traversable sa

  let tree = undefined :: Tree (Int, Int, [Int])
  quickBatch $ functor tree
  quickBatch $ traversable tree