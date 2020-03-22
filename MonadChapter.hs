module MonadChapter where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Foldable (foldMap)

-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg
instance EqProp (Nope a) where
  (=-=) = eq
instance Functor Nope where
  fmap f = const NopeDotJpg
instance Applicative Nope where
  pure = const NopeDotJpg
  fa <*> a = NopeDotJpg
instance Monad Nope where
  return = pure
  ma >>= fa = NopeDotJpg

-- main = do
--   let trigger = undefined :: Nope (Int, String, Int)
--   quickBatch $ functor trigger
--   quickBatch $ applicative trigger
--   quickBatch $ monad trigger

-- 2
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither a b) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]
instance (Eq a, Eq b) => EqProp (BahEither a b) where
  (=-=) = eq
instance Functor (BahEither a) where
  fmap f (PLeft x) = PLeft $ f x
  fmap f (PRight x) = PRight x
instance Applicative (BahEither a) where
  pure = PLeft
  (PLeft fa) <*> (PLeft x) = PLeft (fa x)
  (PRight x) <*> _ = PRight x
  (PLeft fa) <*> (PRight x) = PRight x
instance Monad (BahEither a) where
  return = pure
  (PLeft x) >>= fa = fa x
  (PRight x) >>= fa = (PRight x)

-- main = do
--   let trigger = undefined :: BahEither (Int, String, Int) (Int, String, Int)
--   quickBatch $ functor trigger
--   quickBatch $ applicative trigger
--   quickBatch $ monad trigger

-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity x) = Identity $ f x
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> x = fmap f x
instance Monad Identity where
  return = pure
  (Identity x) >>= fa = fa x

-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Show)
flat :: List (List a) -> List a
flat Nil = Nil
flat (Cons Nil xs) = flat xs
flat (Cons (Cons x xs) xxs) = Cons x (flat (Cons xs xxs))
toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = toList <$> listOf arbitrary
instance (Eq a) => EqProp (List a) where
  (=-=) = eq
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = flat $ Cons (f <$> xs) (Cons (fs <*> xs) Nil)
instance Monad List where
  return = pure
  ma >>= f = flat $ fmap f ma

main = do
  let trigger = undefined :: List (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- 1.
j :: Monad m => m (m a) -> m a
j ma = ma >>= id

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = fmap f ma

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a ma fm = fm <*> ma

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = pure []
meh (x:xs) f = f x >>= (\b -> fmap ([b] ++) (meh xs f))

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType lst = meh lst id

