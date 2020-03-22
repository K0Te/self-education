{-# LANGUAGE InstanceSigs #-}
module StateChapter where

import Control.Monad
import Control.Monad.Trans.State


newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let
      (a, s1) = g s
      b = f a
    in (b, s1)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let
      (a, s1) = g s
      (fab, s2) = f s1
      b = fab a
    in (b, s2)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let
      (a, s1) = f s
      (b, s2) = runMoi (g a) s1
    in (b, s2)

get :: State s s
get = StateT (\s -> pure (s, s))

put :: s -> State s ()
put s = StateT (\_ -> pure((), s) )

exec :: State s a -> s -> s
exec st s = let (a, s1) = runState st s in s1

modify :: (s -> s) -> State s ()
modify f = StateT $ \s -> pure ((), f s)