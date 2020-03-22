{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr
import Data.Word

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IntID = (Identity Int) -> IntToInt -> IntToInt -> Bool

------------------------------

data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary= do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

type IntPair = (Pair Int) -> IntToInt -> IntToInt -> Bool

--------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

type IntTwo = (Two Int Int) -> IntToInt -> IntToInt -> Bool

--------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c) 

type IntThree = (Three Int Int Int) -> IntToInt -> IntToInt -> Bool

--------------------------------------------------------

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three' x y z)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

type IntThree' = (Three' Int Int) -> IntToInt -> IntToInt -> Bool

--------------------------------------------------------

------------ a - Just for fun, otherwise no compile
data Trivial a = Trivial deriving (Eq, Show)

instance Functor Trivial where
  fmap _ _ = Trivial

instance Arbitrary (Trivial a) where
  arbitrary = return Trivial

type IntTrivial = (Trivial Int) -> IntToInt -> IntToInt -> Bool

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

data D = D (Array Word Word) Int Int

main:: IO ()
main = do
  quickCheck (functorCompose' :: IntFC)
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose' :: IntID)
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorCompose' :: IntPair)
  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose' :: IntTwo)
  quickCheck (functorIdentity :: (Two Int Int) -> Bool)
  quickCheck (functorCompose' :: IntThree)
  quickCheck (functorIdentity :: (Three Int Int String) -> Bool)
  quickCheck (functorCompose' :: IntThree')
  quickCheck (functorIdentity :: (Three' Int Int) -> Bool)
  quickCheck (functorCompose' :: IntTrivial)
  quickCheck (functorIdentity :: (Trivial Int) -> Bool)
