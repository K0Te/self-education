{-# LANGUAGE DeriveGeneric #-}

import Data.Semigroup
import Test.QuickCheck
import GHC.Generics


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

newtype Identity a = Identity a deriving (Show, Eq)
instance Semigroup (Identity a) where
  (<>) fst snd = fst

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

data Two a b = Two a b deriving (Show, Eq)

instance Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two a1 b1

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = frequency [ (1, return (BoolConj True))
                          , (1, return (BoolConj False)) ]

data OneOf a b = Fst a | Snd b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (OneOf a b) where
   arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Fst a)),
                (1, return (Snd b))]

instance Semigroup (OneOf a b) where
  (Snd a) <> _ = (Snd a)
  (Fst a) <> x = x

type OneOfForTest = OneOf Int String
type OneOfAssoc = OneOfForTest -> OneOfForTest -> OneOfForTest -> Bool

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b)  where
  (Combine f1) <> (Combine f2) = Combine $ \x -> (f1 x) <> (f2 x)

main :: IO ()
main = do
  -- quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  -- quickCheck (semigroupAssoc :: (Identity Int) -> (Identity Int) -> (Identity Int) -> Bool)
  quickCheck (semigroupAssoc :: (Two Int Int) -> (Two Int Int) -> (Two Int Int) -> Bool)
  quickCheck (semigroupAssoc :: (BoolConj) -> (BoolConj) -> (BoolConj) -> Bool)
  quickCheck (semigroupAssoc :: OneOfAssoc)
  quickCheck (semigroupAssoc :: OneOfAssoc)
