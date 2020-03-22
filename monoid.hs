{-# LANGUAGE DeriveGeneric #-}

import Data.Semigroup
import Test.QuickCheck
import GHC.Generics

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

------------------------------------------------------------

monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
monoidLeftIdentity m = (m `mappend` mempty) == m

monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
monoidRightIdentity m = (mempty `mappend` m) == m

main :: IO ()
main = do
  -- quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
