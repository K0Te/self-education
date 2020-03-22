module Monad where

data Nope a = NopeDotJpg

instance Functor Nope where
  _ `fmap`  _ = NopeDotJpg

instance Applicative Nope where
  _ <*> _ = NopeDotJpg
  pure x = NopeDotJpg

instance Monad Nope where
  return x = NopeDotJpg
  _ >>= _ = NopeDotJpg

data MyEither b a = MLeft a | MRight b

instance Functor (MyEither a) where
  fmap f (MLeft x) = MLeft (f x)
  fmap f (MRight x) = MRight x

instance Applicative (MyEither a) where
  pure x = MLeft x
  (MLeft f) <*> (MLeft x) = MLeft (f x)
  (MRight e) <*> _ = MRight e
  _ <*> (MRight x) = MRight x

instance Monad (MyEither a) where
  (MLeft x) >>= f = f x
  (MRight x) >>= f = MRight x

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

data List a = Nil | Cons a (List a)

app:: List a -> List a -> List a
app Nil x = x
app (Cons x xs) lst = Cons x (app xs lst)

mmconcat:: List (List a) -> List a
mmconcat Nil = Nil
mmconcat (Cons xs xss) = xs `app` (mmconcat xss)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = (fmap f xs) `app` (fs <*> xs)

instance Monad List where
  xs >>= f = mmconcat $ fmap f xs

j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (\a -> pure $ f a)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 fab ma mb = ma >>= (\a -> mb >>= (\b -> pure $ fab a b))

a :: Monad m => m a -> m (a -> b) -> m b
a ma mfa = mfa <*> ma

mToLst :: Monad m => [m x] -> m [x]
--mToLst = foldr (\acc val -> acc >>= (\x -> val >>= (\y -> return (y:x))))
--  (return [])

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = mToLst $ fmap f xs

