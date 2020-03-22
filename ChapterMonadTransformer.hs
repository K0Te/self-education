{-# LANGUAGE InstanceSigs #-}
module ChapterMonadTransformer where

import Control.Monad
import Data.Maybe
import Data.Either

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma


instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= fa = MaybeT $ (\x -> join $ fmap (fromMaybe (pure Nothing)) x) ((fmap . fmap) (runMaybeT.fa) mma)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f a = EitherT $ (fmap . fmap) f (runEitherT a)


instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (pure a)
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= fa = EitherT $ (\x -> join $ fmap (either (pure . Left) id) x) ((fmap . fmap) (runEitherT.fa) mea)

swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT mea) = join $ fmap (either fa fb) mea

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f a = ReaderT $ \r -> fmap f (runReaderT a r)

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ \r -> (pure a)
  (ReaderT mfa) <*> (ReaderT ma) = ReaderT $ \r -> (mfa r) <*> (ma r)

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT ma) >>= fa = ReaderT $ \r -> join $ fmap (\a -> runReaderT (fa a) r) (ma r)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT rs) = StateT $ \s -> fmap (\(a, s1) -> (f a, s1)) (rs s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT sfa) <*> (StateT sa) = StateT $ \s ->
    let m1 = sfa s
    in join $ fmap (\(fa, s1) -> fmap (\(a, s2) -> (fa a, s2)) (sa s1)) m1


instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT ma) >>= fa = StateT $ \s -> join $ fmap (\(a, s1) -> runStateT (fa a) s1) (ma s)

main = putStrLn "Hello World"











