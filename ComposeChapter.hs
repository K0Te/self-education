{-# LANGUAGE InstanceSigs #-}

module ComposeChapter where

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose $ (fmap . fmap) f x

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ (pure . pure) a
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgy) <*> (Compose fga) = Compose $ pure (\gy ga -> gy <*> ga) <*> fgy <*> fga

-- 25.6.1
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = foldMap (\ga -> foldMap f ga) fga

-- 25.6.2
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  sequenceA :: Applicative x => Compose f g (x a) -> x (Compose f g a)
  sequenceA (Compose fg_xa) = Compose <$> (sequenceA $ sequenceA <$> fg_xa)