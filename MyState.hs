{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module MyState where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (a, s1) = g s
    in (f a, s1) 

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s ->
      let (ff, s1) = f s
          (a, s2) = g s1
      in (ff a, s2)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, s1) = f s
        mb = g a
        (b, s2) = runMoi mb s1
    in (b, s2)

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..100]

get :: Moi s s
get = Moi $ \s -> (s, s)

put:: s -> Moi s ()
put s = Moi $ \ss -> ((), s)

exec:: Moi s a -> s -> s
exec (Moi m) s = let (a, s) = m s in s

eval:: Moi s a -> s -> a
eval (Moi m) s = let (a, s) = m s in a

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
